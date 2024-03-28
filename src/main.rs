use rfc3986::URI;

fn main() {
    let uris = vec! [
        "http://httpbin.org/get?a=1&b=2#hash",
    ];
    for uri in uris {
        println!("{}", uri);
        let u = URI::try_parse(uri);
        println!("{:#?}", u);
    }
    
}
