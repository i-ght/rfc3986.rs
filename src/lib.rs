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

use std::{collections::VecDeque, error::Error, fmt::Display};

#[derive(Debug)]
pub enum ParseURIError {
    ErroneousScheme,
    ErroneousPath
}

/* [ userinfo "@" ] host [ ":" port ] */
#[derive(Debug, PartialEq, Eq)]
pub struct Authority {
    host: String,
    port: u16,
    user_info: Option<String>,
    value: String
}

#[derive(Debug)]
pub enum ParseAuthorityError {
    InvalidPort,
    InvalidIPV6
}
/*
      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

      hier-part   = "//" authority path-abempty
                  / path-absolute
                  / path-rootless
                  / path-empty 
*/
#[derive(Debug, PartialEq, Eq)]
pub struct URI {
    scheme: String,
    authority: Option<String>,
    /* authority: Option<Authority>, */
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
struct URIComponents<'a> {
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
            find_next_opt_component(URIQueryOrFragmentOrEither::Query, s)
            .or(find_next_opt_component(URIQueryOrFragmentOrEither::Fragment, s))
    }
}

fn find_auth_terminator(
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

/* 
    URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
    scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
*/
fn parse_scheme<'a>(
    split: &Vec<&'a str>,
    components: URIComponents<'a>
) -> Result<URIComponents<'a>, ParseURIError> {
    if split.len() != 2 {
        return Err(ParseURIError::ErroneousScheme)
    }

    if split[0].is_empty() {
        return Err(ParseURIError::ErroneousScheme);
    }
    
    let scheme = Some(split[0]);
    Ok(URIComponents { scheme, ..components })
}

/*
      URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
      authority   = [ userinfo "@" ] host [ ":" port ]

    The authority component is preceded by a double slash ("//") and is
    terminated by the next slash ("/"), question mark ("?"), or number
    sign ("#") character, or by the end of the URI.

    If a URI contains an authority component, then the path component
    must either be empty or begin with a slash ("/") character.
*/
fn parse_authority<'a>(
    split: &Vec<&'a str>,
    components: URIComponents<'a>
) -> URIComponents<'a> {
    if split.len() != 2 {
        return components;
    }
    let authority_terminator = find_auth_terminator(split[1]);
    
    let head = split[1];
    let authority = Some(&head[..authority_terminator]);

    URIComponents { authority, ..components }
}

/*
    The path is terminated
    by the first question mark ("?") or number sign ("#") character, or
    by the end of the URI.
*/
fn parse_path<'a>(
    split: &Vec<&'a str>,
    components: URIComponents<'a>
) -> Result<URIComponents<'a>, ParseURIError> {
    if split.len() != 2 {
        if components.tail.is_empty() {
            return Err(ParseURIError::ErroneousPath)
        }
        let path = Some(components.tail);
        return Ok(URIComponents { path, ..components });
    }
    
    let head = split[0];
    let tail = &components.tail[head.len()..];    
    let path_terminator =
        match find_next_opt_component(URIQueryOrFragmentOrEither::Either, tail) {
            Some((_, i)) => i,
            None => tail.len()
        };
    let path = Some(&tail[..path_terminator]);

    Ok(URIComponents { path, ..components })
}

/*
    The query component is indicated by the first question
    mark ("?") character and terminated by a number sign ("#") character
    or by the end of the URI.
*/
fn parse_query<'a>(
    split: &Vec<&'a str>,
    components: URIComponents<'a>
) -> URIComponents<'a> {
    if split.len() != 2 {
        return components;
    }

    let head = split[1];    
    let query_terminator =
        match find_next_opt_component(URIQueryOrFragmentOrEither::Fragment, head) {
            Some((_, i)) => i,
            None => head.len()
        };
    let query = Some(&head[..query_terminator]);

    URIComponents { query, ..components }
}

/*
    A fragment identifier component is indicated by the presence of a
    number sign ("#") character and terminated by the end of the URI.
*/
fn parse_fragment<'a>(
    split: &Vec<&'a str>,
    components: URIComponents<'a>
) -> URIComponents<'a> {
    if split.len() != 2 {
        return components;
    }
    let fragment = Some(split[1]);
    URIComponents { fragment, ..components }
}

/*
    URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
*/
fn advance_phase<'a>(
    components: URIComponents<'a>,
    delimiter: &str
) -> Result<URIComponents<'a>, ParseURIError> {
    let split: Vec<&str> =
        components.tail
            .splitn(2, delimiter)
            .collect();
    let phase =
        match delimiter {
            ":" => parse_scheme(&split, components)?,
            "//" => parse_authority(&split, components),
            "/" => parse_path(&split, components)?,
            "?" => parse_query(&split, components),
            "#" => parse_fragment(&split, components),
            _ => unreachable!()
        };
        
    let tail = *split.last().unwrap();
    Ok(URIComponents { tail, ..phase })
}

fn structure_components(
    supposed_uri: &str
) -> Result<URIComponents, ParseURIError> {
    /*
        The generic syntax uses the slash ("/"), question mark ("?"), and
        number sign ("#") characters to delimit components that are
        significant 
   */
    let interest = vec! [":", "//", "/", "?", "#"];
    let mut interest = VecDeque::from(interest);
    let initial_phase = URIComponents::new(supposed_uri);

    let mut phase = initial_phase;
    while let Some(delimiter) = interest.pop_front() {
        phase = advance_phase(phase, delimiter)?;
    };
    Ok(phase)
}

impl<'a> URIComponents<'a> {
    fn new(uri: &str) -> URIComponents {
        URIComponents {
            tail: uri,
            scheme: None,
            authority: None,
            path: None,
            query: None,
            fragment: None
        }
    }
}

impl<'a> From<URIComponents<'a>> for URI {
    fn from(components: URIComponents) -> Self {
        URI {
            scheme: String::from(components.scheme.unwrap()),
            authority: components.authority.map(str::to_string),
            path: String::from(components.path.unwrap()),
            query: components.query.map(str::to_string),
            fragment: components.fragment.map(str::to_string)
        }
    }
}

impl TryFrom<&str> for URI {
    type Error = ParseURIError;
    fn try_from(value: &str) -> Result<URI, Self::Error> {
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
            authority: authority.map(str::to_string),
            path: String::from(path),
            query: query.map(str::to_string),
            fragment: fragment.map(str::to_string)
        }
    }
}

impl Display for ParseURIError {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> std::fmt::Result {
        match self {
            ParseURIError::ErroneousScheme => write!(f, "scheme not found."),
            ParseURIError::ErroneousPath => write!(f, "path not found."),
        }
    }
}

impl Error for ParseURIError { }

impl<'a> Display for ParseAuthorityError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseAuthorityError::InvalidPort => 
                write!(
                    f,
                    "error parsing port into number."
                ),
            ParseAuthorityError::InvalidIPV6 =>
                write!(
                    f, 
                    "error parsing ipv6 address. no closing ] found."
                )
        }
    }
}

impl<'a> Error for ParseAuthorityError { }

impl TryFrom<(&str, u16)> for Authority {
    type Error = ParseAuthorityError;

    fn try_from((value, default_port): (&str, u16)) -> Result<Self, Self::Error> {
        let index_of_at = value.find('@');
/*
        userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
*/
        let (user_info, tail) =
            match index_of_at {
                Some(at) =>
                    (Some(String::from(&value[..at])), &value[at..]),
                None =>
                    (None, value)
            };

/* 
        A host identified by an Internet Protocol literal address, version 6
        [RFC3513] or later, is distinguished by enclosing the IP literal
        within square brackets ("[" and "]").  This is the only place where
        square bracket characters are allowed in the URI syntax.  In
        anticipation of future, as-yet-undefined IP literal address formats,
        an implementation may use an optional version flag to indicate such a
        format explicitly rather than rely on heuristic determination.

            IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
*/
        let tail =
            if tail.starts_with('[') {
                match tail.find(']') {
                    Some(i) => &tail[i+1..],
                    None => return Err(ParseAuthorityError::InvalidIPV6),
                }
            } else {
                tail
            };

        let index_of_colon = tail.find(':');
        let (host, port) =
            match index_of_colon {
                Some(colon) => {
                    let host = &tail[..colon];
                    let port =
                    tail[colon+1..]
                            .parse::<u16>()
                            .map_err(|_e| ParseAuthorityError::InvalidPort)?;
                    (String::from(host), port)
                },
                None =>
                    (String::from(tail), default_port),
            };

        Ok(
            Authority {
                host,
                port,
                user_info,
                value: String::from(value)
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::URI;

    #[test]
    fn exec() {
        let uris: Vec<(&str, Option<URI>)> = 
            vec! [
                (
                    "http://httpbin.org/a/b/c?123#doreme",
                    Some(URI::from_components("http", Some("httpbin.org"), "/a/b/c", Some("123"), Some("doreme")))
                ),
                (
                    "http://httpbin.org/a/b/c/?123#doreme",
                    Some(URI::from_components("http", Some("httpbin.org"), "/a/b/c/", Some("123"), Some("doreme")))
                ),
                (
                    "http://httpbin.org/get?a=1&b=2&c=3#hash",
                    Some(URI::from_components("http", Some("httpbin.org"), "/get", Some("a=1&b=2&c=3"), Some("hash")))
                ),
                (
                    "ftp://ftp.is.co.za/rfc/rfc1808.txt",
                    Some(URI::from_components("ftp", Some("ftp.is.co.za"), "/rfc/rfc1808.txt", None, None))
                ),
                (
                    "http://www.ietf.org/rfc/rfc2396.txt",
                    Some(URI::from_components("http", Some("www.ietf.org"), "/rfc/rfc2396.txt", None, None))
                ),
                (
                    "ldap://[2001:db8::7]/c=GB?objectClass?one",
                    Some(URI::from_components("ldap", Some("[2001:db8::7]"), "/c=GB", Some("objectClass?one"), None))
                ),
                (
                    "mailto:John.Doe@example.com",
                    Some(URI::from_components("mailto", None, "John.Doe@example.com", None, None))
                ),
                (
                    "news:comp.infosystems.www.servers.unix",
                    Some(URI::from_components("news", None, "comp.infosystems.www.servers.unix", None, None))
                ),
                (
                    "tel:+1-816-555-1212",
                    Some(URI::from_components("tel", None, "+1-816-555-1212", None, None))
                ),
                (
                    "telnet://192.0.2.16:80/",
                    Some(URI::from_components("telnet", Some("192.0.2.16:80"), "/", None, None))
                ),
                (
                    "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
                    Some(URI::from_components("urn", None, "oasis:names:specification:docbook:dtd:xml:4.1.2", None, None))
                )
            ];

        for (uri_s, expected_value) in uris {
            let uri = URI::try_parse(uri_s);
            assert_eq!(uri, expected_value);
        }
    }
}
