use rfc3986::URI;
use regex::{Regex, RegexBuilder};

fn regex() {
    let scheme_authority_path = r"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?";
    let mut re_ctor = RegexBuilder::new(scheme_authority_path);
    let re_ctor =
        re_ctor
            .case_insensitive(true);
    let re = re_ctor.build().expect("regex build");

}

fn main() {

    let uris = vec! [
        "http://httpbin.org/1/2/3?a=0&b=1&c=2",
        "http://httpbin.org/1/2/3?q1=1&q2=2#okayy",
        "http://httpbin.org/1/2/3",
        "ldap://[2001:db8::7]/c=GB?objectClass?one?two?three#hello",
        "ldap://[2001:db8::7]/c=GB?objectClass?#helloone?two?three",
        "ldap://[2001:db8::7]/c=GB?#helloobjectClass?one?two?three#hello",
        
        "ldap://[2001:db8::7]/c=GB?objectClass?one?two?three",
        "ftp://ftp.is.co.za/rfc/rfc1808.txt",
        "http://www.ietf.org/rfc/rfc2396.txt",
        "mailto:John.Doe@example.com",
        "news:comp.infosystems.www.servers.unix",
        "tel:+1-816-555-1212",
        "telnet://192.0.2.16:80/",
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    ];
    for uri in uris {
        let u = URI::parse(uri);
        println!("{:#?}", u);
    }
    
}
