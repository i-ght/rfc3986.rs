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

use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum URIParseError {
    NoScheme,
    NoPath
}

#[derive(Debug, PartialEq, Eq)]
pub struct URI {
    scheme: String,
    authority: Option<String>,
    path: String,
    query: Option<String>,
    fragment: Option<String>
}

enum URIQueryOrFragment {
    Query,
    Fragment
}

enum URIQueryOrFragmentOrEither {
    Query,
    Fragment,
    Either
}

#[derive(Debug)]
struct URIPhase<'a> {
    tail: &'a str,
    scheme: Option<&'a str>,
    authority: Option<&'a str>,
    path: Option<&'a str>,
    query: Option<&'a str>,
    fragment: Option<&'a str>
}

fn find_next_opt_component(
    q_or_f: URIQueryOrFragmentOrEither,
    s: &str
) -> Option<(URIQueryOrFragment, usize)> {
    match q_or_f {
        URIQueryOrFragmentOrEither::Query => 
            s
                .find('?')
                .map(|i| (URIQueryOrFragment::Query, i)),
        URIQueryOrFragmentOrEither::Fragment =>
            s
                .find('#')
                .map(|i| (URIQueryOrFragment::Fragment, i)),
        URIQueryOrFragmentOrEither::Either =>
            match s.find('?') {
                Some(i) =>
                    Some((URIQueryOrFragment::Query, i)),
                None => match s.find('#') {
                    Some(i) => Some((URIQueryOrFragment::Fragment, i)),
                    None => None
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

fn advance_phase_scheme(
    phase: URIPhase
) -> Result<URIPhase, URIParseError> {
    let tail = phase.tail;

    /* first occurrence of ':' starting from left */
    let colon = 
        tail
            .find(":")
            .ok_or(URIParseError::NoScheme)?;

    if (&tail[..colon]).is_empty() {
        return Err(URIParseError::NoScheme);
    }

    let scheme = Some(&tail[..colon]);
    let tail = &tail[colon+1..];

    Ok(
        URIPhase { scheme, tail, ..phase }
    )
}

fn advance_phase_auth(
    phase: URIPhase
) -> Result<URIPhase, URIParseError> {
/*
    The authority component is preceded by a double slash ("//") and is
    terminated by the next slash ("/"), question mark ("?"), or number
    sign ("#") character, or by the end of the URI.
*/
    if phase.tail.starts_with("//") {
        let mut tail = phase.tail;

        tail = &tail[2..];

        let auth_delim = find_auth_delim(tail);
        let authority = Some(&tail[..auth_delim]);
        
        tail = &tail[auth_delim..];

        return Ok(
            URIPhase { tail, authority, ..phase }
        )
    }
    
    Ok(
        URIPhase { ..phase }
    ) 
}

fn advance_phase_query_frag(
    phase: URIPhase
) -> Result<URIPhase, URIParseError> {
    
    if phase.tail.is_empty() {
        return Ok(URIPhase { ..phase });
    }

    let mut tail = phase.tail;

    while let Some((q_or_f, i)) =
        find_next_opt_component(
            URIQueryOrFragmentOrEither::Either,
            tail
    ) {
        let opposite = q_or_f.opposite();
        let opposite_witheither =
            match opposite {
                URIQueryOrFragment::Query => URIQueryOrFragmentOrEither::Query,
                URIQueryOrFragment::Fragment => URIQueryOrFragmentOrEither::Fragment,
            };

        if let Some((_, j)) = find_next_opt_component(opposite_witheither,tail) {
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

            return Ok(
                URIPhase { 
                    fragment: Some(fragment),
                    query: Some(query), 
                    tail,
                    ..phase 
                }
            );
        } else {
            let value = &tail[i+1..];
            tail = &tail[i+1+value.len()..];
            match q_or_f {
                URIQueryOrFragment::Query =>
                    return Ok(
                        URIPhase {
                            tail,
                            query: Some(value),
                            ..phase
                        }
                    ),
                URIQueryOrFragment::Fragment =>
                    return Ok(
                        URIPhase {
                            tail,
                            fragment: Some(value),
                            ..phase 
                        }
                    ),
            }
        }
    }

    Ok(
        URIPhase { tail, ..phase }
    )
}

fn advance_phase_path(
    phase: URIPhase
) -> Result<URIPhase, URIParseError> {
    
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

    if split.len() == 0 {
        return Err(
            URIParseError::NoPath
        );
    }

    let tail_unit = *split.last().unwrap();
    match find_next_opt_component(URIQueryOrFragmentOrEither::Either, tail_unit) {
        Some((_component, index)) => {
            let index = index + tail.len() - tail_unit.len();

            let head = &tail[..index];
            if head.is_empty() {
                return Err(URIParseError::NoPath)
            }
            
            let path = Some(head);
            tail = &tail[index..];

            Ok(
                URIPhase { tail, path, ..phase }
            )
        },
        None => {
            let path = Some(tail);
            let tail = &phase.tail[phase.tail.len()..];

            Ok(
                URIPhase { tail, path, ..phase }
            )
        }
    }

}

fn phase_shift<'a>(
    phase_changes: &[fn(URIPhase) -> Result<URIPhase, URIParseError>],
    initial_phase: URIPhase<'a>
) -> Result<URIPhase<'a>, URIParseError> {
    let mut current_phase = initial_phase;
    for change in phase_changes {
        current_phase = change(current_phase)?;
    }
    Ok(current_phase)
}

fn structure_components(
    uri: &str
) -> Result<URIPhase, URIParseError> {
    let phase = URIPhase::new(uri);

    let phase_changes = vec!
        [ advance_phase_scheme,
          advance_phase_auth,
          advance_phase_path,
          advance_phase_query_frag ];

    phase_shift(
        &phase_changes,
        phase
    )
}

fn str_opt_to_string(a: Option<&str>) -> Option<String> {
    match a {
        Some(str) => Some(String::from(str)),
        None => None
    }
}

impl URIQueryOrFragment {
    fn opposite(&self) -> URIQueryOrFragment {
        match self {
            URIQueryOrFragment::Query => URIQueryOrFragment::Fragment,
            URIQueryOrFragment::Fragment => URIQueryOrFragment::Query,
        }
    }
}

impl<'a> URIPhase<'a> {
    fn new(uri: &str) -> URIPhase {
        URIPhase {
            tail: uri,
            scheme: None,
            authority: None,
            path: None,
            query: None,
            fragment: None
        }
    }
}

impl<'a> From<URIPhase<'a>> for URI {
    fn from(components: URIPhase) -> Self {
        URI {
            scheme: String::from(components.scheme.unwrap()),
            authority: str_opt_to_string(components.authority),
            path: String::from(components.path.unwrap()),
            query: str_opt_to_string(components.query),
            fragment: str_opt_to_string(components.fragment)
        }
    }
}

impl TryFrom<&str> for URI {
    type Error = URIParseError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let phase = structure_components(value)?;
        Ok(URI::from(phase))
    }
}

impl URI {
    pub fn try_parse(uri: &str) -> Option<URI> {
        URI::try_from(uri).ok()
    }
    pub fn from_components(
        scheme: &str,
        authority: Option<&str>,
        path: &str,
        query: Option<&str>,
        fragment: Option<&str>
    ) -> URI {
        URI {
            scheme: String::from(scheme),
            authority: str_opt_to_string(authority),
            path: String::from(path),
            query: str_opt_to_string(query),
            fragment: str_opt_to_string(fragment)
        }
    }
}

impl Display for URIParseError {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> std::fmt::Result {
        match self {
            URIParseError::NoScheme => write!(f, "uri scheme not found."),
            URIParseError::NoPath => write!(f, "uri path not found."),
        }
    }
}

impl Error for URIParseError { }

#[cfg(test)]
mod tests {
    use super::URI;

    #[test]
    fn exec() {
        let uris: Vec<(&str, Option<URI>)> = 
            vec! [
                ("ftp://ftp.is.co.za/rfc/rfc1808.txt", Some(URI::from_components("ftp", Some("ftp.is.co.za"), "/rfc/rfc1808.txt", None, None))),
                ("http://www.ietf.org/rfc/rfc2396.txt", Some(URI::from_components("http", Some("www.ietf.org"), "/rfc/rfc2396.txt", None, None))),
                ("ldap://[2001:db8::7]/c=GB?objectClass?one", Some(URI::from_components("ldap", Some("[2001:db8::7]"), "/c=GB", Some("objectClass?one"), None))),
                ("mailto:John.Doe@example.com", Some(URI::from_components("mailto", None, "John.Doe@example.com", None, None))),
                ("news:comp.infosystems.www.servers.unix", Some(URI::from_components("news", None, "comp.infosystems.www.servers.unix", None, None))),
                ("tel:+1-816-555-1212", Some(URI::from_components("tel", None, "+1-816-555-1212", None, None))),
                ("telnet://192.0.2.16:80/", Some(URI::from_components("telnet", Some("192.0.2.16:80"), "/", None, None))),
                ("urn:oasis:names:specification:docbook:dtd:xml:4.1.2", Some(URI::from_components("urn", None, "oasis:names:specification:docbook:dtd:xml:4.1.", None, None)))
            ];

        for (uri_s, expected_value) in uris {
            let uri = URI::try_parse(uri_s);
            assert_eq!(uri, expected_value)
        }
    }
}