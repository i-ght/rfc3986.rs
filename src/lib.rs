/* 
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

use std::collections::HashSet;

#[derive(Debug)]
enum AdvancePhaseError {
    Scheme,
    Path
}

fn find_next_opt_component(
    q_or_f: URIQueryOrFragmentOrEither,
    s: &str
) -> (URIQueryOrFragment, Option<usize>) {
    match q_or_f {
        URIQueryOrFragmentOrEither::Query => (URIQueryOrFragment::Query, s.find('?')),
        URIQueryOrFragmentOrEither::Fragment => (URIQueryOrFragment::Fragment, s.find('#')),
        URIQueryOrFragmentOrEither::Either =>
            match s.find('?') {
                Some(i) => (URIQueryOrFragment::Query, Some(i)),
                None => match s.find('#') {
                    Some(i) => (URIQueryOrFragment::Fragment, Some(i)),
                    None => (URIQueryOrFragment::Fragment, None)
                }
            }
    }
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
) -> Option<(URIQueryOrFragment, usize)> {
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
                    '?' => URIQueryOrFragment::Query,
                    '#' => URIQueryOrFragment::Fragment,
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

    let mut tail = phase.tail;

    while let (q_or_f, Some(i)) = find_next_opt_component(URIQueryOrFragmentOrEither::Either, tail) {
        
        let opposite = q_or_f.opposite();
        let opposite_witheither =
            match opposite {
                URIQueryOrFragment::Query => URIQueryOrFragmentOrEither::Query,
                URIQueryOrFragment::Fragment => URIQueryOrFragmentOrEither::Fragment,
            };


        if let (_, Some(j)) = find_next_opt_component(opposite_witheither,tail) {
            let this_side = &tail[i+1..j];
            let that_side = &tail[j+1..];
            let (fragment, query) = 
                match (q_or_f, opposite) {
                    (URIQueryOrFragment::Fragment, URIQueryOrFragment::Query) =>
                        (this_side, that_side),
                    (URIQueryOrFragment::Query, URIQueryOrFragment::Fragment) =>
                        (that_side, this_side),
                    _ =>
                        unreachable!()
                };
            
            tail = &tail[j..];
            let phase = URIPhase 
                { fragment: Some(fragment),
                  query: Some(query), 
                  tail,
                  ..phase };

            return Ok(phase);
        } else {

            let value = &tail[i+1..];
            tail = &tail[i+1+value.len()..];
            match q_or_f {
                URIQueryOrFragment::Query =>
                    return Ok(URIPhase { tail, query: Some(value), ..phase }),
                URIQueryOrFragment::Fragment =>
                    return Ok(URIPhase { tail, fragment: Some(value), ..phase }),
            }
        }
        
    }

    Ok(URIPhase { tail, ..phase })
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

enum URIQueryOrFragment {
    Query = 1,
    Fragment = 2
}

impl URIQueryOrFragment {
    fn opposite(&self) -> URIQueryOrFragment {
        match self {
            URIQueryOrFragment::Query => URIQueryOrFragment::Fragment,
            URIQueryOrFragment::Fragment => URIQueryOrFragment::Query,
        }
    }
}

enum URIQueryOrFragmentOrEither {
    Query,
    Fragment,
    Either
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

impl URIQueryOrFragment {
    pub fn delimiter(&self) -> char {
        match self {
            URIQueryOrFragment::Query => '?',
            URIQueryOrFragment::Fragment => '#',
        }
    }
}