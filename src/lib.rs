/* 
2.2.  Reserved Characters

   URIs include components and subcomponents that are delimited by
   characters in the "reserved" set.  These characters are called
   "reserved" because they may (or may not) be defined as delimiters by
   the generic syntax, by each scheme-specific syntax, or by the
   implementation-specific syntax of a URI's dereferencing algorithm.
   If data for a URI component would conflict with a reserved
   character's purpose as a delimiter, then the conflicting data must be
   percent-encoded before the URI is formed.

Berners-Lee, et al.         Standards Track                    [Page 12]


RFC 3986                   URI Generic Syntax               January 2005


      reserved    = gen-delims / sub-delims

      gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"

      sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
                  / "*" / "+" / "," / ";" / "="

2.3.  Unreserved Characters

   Characters that are allowed in a URI but do not have a reserved
   purpose are called unreserved.  These include uppercase and lowercase
   letters, decimal digits, hyphen, period, underscore, and tilde.

      unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"

3.  Syntax Components

   The generic URI syntax consists of a hierarchical sequence of
   components referred to as the scheme, authority, path, query, and
   fragment.

      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

      hier-part   = "//" authority path-abempty
                  / path-absolute
                  / path-rootless
                  / path-empty 
    The following are two example URIs
     and their component parts:

         foo://example.com:8042/over/there?name=ferret#nose
         \_/   \______________/\_________/ \_________/ \__/
          |           |            |            |        |
       scheme     authority       path        query   fragment
          |   _____________________|__
         / \ /                        \
         urn:example:animal:ferret:nose

*/


/* 
URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

The scheme and path components are required, though the path may be
empty (no characters).  When authority is present, the path must
either be empty or begin with a slash ("/") character.  When
authority is not present, the path cannot begin with two slash
characters ("//"). 

The following are two example URIs and their component parts:

         foo://example.com:8042/over/there?name=ferret#nose
         \_/   \______________/\_________/ \_________/ \__/
          |           |            |            |        |
       scheme     authority       path        query   fragment
          |   _____________________|__
         / \ /                        \
         urn:example:animal:ferret:nose

*/


#[derive(Debug)]
enum AdvancePhaseError {
    Scheme,
    Path
}

fn find_auth_delim(
    uri: &str
) -> usize {
    let mut results: Vec<char> = Vec::with_capacity(3);
    
    for c in uri.chars() {
        if c == '/' || c == '?' || c == '#' {
            if !results.contains(&c) {
                results.push(c);
            }
        }
    }

    for c in results {
        if let Some(i) = uri.find(c) {
            return i;
        }
    }
    
    uri.len()
}

fn find_query_or_frag(
    tail: &str,
) -> Option<(URIOptionalComponent, usize)> {
    let mut results: Vec<char> = Vec::with_capacity(3);
    
    for c in tail.chars() {
        if (c == '?' || c == '#') && !results.contains(&c)  {
            results.push(c);
        }
        
    }

    for c in results {
        if let Some(i) = tail.find(c) {
            let comp_type = 
                match c {
                    '?' => URIOptionalComponent::Query,
                    '#' => URIOptionalComponent::Fragment,
                    _ => unreachable!()
                };
            return Some((comp_type, i));
        }
    }

    None
}

fn advance_phase_scheme<'a>(
    phase: URIPhase
) -> Result<URIPhase, AdvancePhaseError> {
    let tail = phase.tail;

    /* first occurence of ':' starting from left */
    let colon = 
        tail
            .find(":")
            .ok_or(AdvancePhaseError::Scheme)?;

    let scheme = Some(&tail[..colon]);

    let tail = &tail[colon+1..];

    Ok(URIPhase { scheme, tail, ..phase })
}

fn advance_phase_auth(
    phase: URIPhase
) -> Result<URIPhase, AdvancePhaseError> {
/*
    The authority component is preceded by a double slash ("//") and is
    terminated by the next slash ("/"), question mark ("?"), or number
    sign ("#") character, or by the end of the URI.
*/
    let mut tail = phase.tail;

    if tail.starts_with("//") {
        tail = &tail[2..];

        let auth_delim = find_auth_delim(tail);

        let authority = Some(&tail[..auth_delim]);
        
        tail = &tail[auth_delim..];

        return Ok(URIPhase { tail, authority, ..phase })
    }
    
    Ok(URIPhase { ..phase }) 
}

fn advance_phase_query_frag(
    phase: URIPhase
) -> Result<URIPhase, AdvancePhaseError> {

    if phase.tail.is_empty() {
        return Ok(URIPhase { ..phase });
    }

    let opt =
        match find_query_or_frag(phase.tail) {
            Some((component, component_head)) => {
                let component_tail =
                    match find_query_or_frag(&phase.tail[component_head+1..]) {
                        Some((_, index)) => index,
                        None => phase.tail.len() -  1,
                    };
                let value = &phase.tail[component_head+1..component_tail+1];
                let tail = &phase.tail[component_tail+1..];
                Some((component, value, tail))
            },
            None =>
                None
        };

    let ret =
        match opt {
            Some((component, value, tail)) => {
                match component {
                    URIOptionalComponent::Query => {
                        let query =
                            match phase.query {
                                None => Some(value),
                                query => query
                            };
                        URIPhase {tail, query, ..phase }
                    }
                    ,
                    URIOptionalComponent::Fragment => {
                        let fragment =
                            match phase.fragment {
                                None => Some(value),
                                fragment => fragment
                            };

                        URIPhase {tail, fragment, ..phase }
                    }
                }
            },
            None =>
                URIPhase { ..phase }
        };

    Ok(ret)
}


fn advance_phase_path<'a, 'b>(
    phase: URIPhase
) -> Result<URIPhase, AdvancePhaseError> {
    
    let mut tail = phase.tail;
    let split: Vec<&str> =
        tail
            .split("/")
            .filter_map(|s| if s.is_empty() { None } else { Some(s) })
            .collect();
    
    if split.len() == 0 && phase.tail == "/" {
        let path = Some(phase.tail);
        return Ok(URIPhase { path, ..phase });
    }

    let tail_unit = *split.last().unwrap();
    match find_query_or_frag(tail_unit) {
        Some((_component, index)) => {
            
            let index = index + tail.len() - tail_unit.len();

            /* account for removed / characters */
            let mut len_of_head = split.len();
            for i in 0..split.len()-1 {
                len_of_head += split[i].len();
            };
            let head = &tail[..index];
            if head.is_empty() {
                return Err(AdvancePhaseError::Path)
            }
            
            let path = Some(head);
            tail = &tail[index..];
            Ok(URIPhase {tail, path, ..phase})
        },
        None => {
            let path = Some(tail);
            let tail = &phase.tail[phase.tail.len()..];
            Ok(URIPhase {tail, path, ..phase})
        },
    }

}

fn phase_shift<'a>(
    phase_changes: &[fn(URIPhase) -> Result<URIPhase, AdvancePhaseError>],
    current_phase: URIPhase<'a>,
    index: usize,
) -> Result<URIPhase<'a>, AdvancePhaseError> {
    if index >= phase_changes.len() {
        Ok(current_phase)
    } else {
        let next_phase = phase_changes[index](current_phase)?;
        phase_shift(phase_changes, next_phase, index + 1)
    }
}


fn structure_components(
    uri: &str
) -> Result<URIPhase, AdvancePhaseError> {

    let phase = URIPhase::new(uri);

    let phase_changes = vec!
        [ advance_phase_scheme,
          advance_phase_auth,
          advance_phase_path,
          advance_phase_query_frag,
          advance_phase_query_frag ];

    phase_shift(
        &phase_changes,
        phase,
        0
    )
}


#[derive(Debug)]
pub struct URI {
    scheme: String,
    authority: Option<String>,
    path: String,
    query: Option<String>,
    fragment: Option<String>
}

pub enum URIOptionalComponent {
    Query,
    Fragment
}

pub enum URIComponent {
    Scheme,
    Authority,
    Path,
    Query,
    Fragment
}

#[derive(Debug)]
struct URIPhase<'a> {
    input: &'a str,
    tail: &'a str,
    scheme: Option<&'a str>,
    authority: Option<&'a str>,
    path: Option<&'a str>,
    query: Option<&'a str>,
    fragment: Option<&'a str>
}

impl<'a> URIPhase<'a> {
    fn new(uri: &str) -> URIPhase {
        URIPhase {
            input: uri,
            tail: uri,
            scheme: None,
            authority: None,
            path: None,
            query: None,
            fragment: None
        }
    }
}

impl URI {

    pub fn parse(uri: &str) -> Option<URI> {
        let phase =
            structure_components(uri).ok()?;
        println!("{:#?}", phase);


        None
    }
}

impl URIOptionalComponent {
    pub fn delimiter(&self) -> char {
        match self {
            URIOptionalComponent::Query => '?',
            URIOptionalComponent::Fragment => '#',
        }
    }
}