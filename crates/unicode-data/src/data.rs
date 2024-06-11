pub mod unicode_15_1_0 {
    pub const UNICODE_DATA_TXT: &str =
        include_str!("../assets/UnicodePublic/15.1.0/ucd/UnicodeData.txt");
}

pub mod mappings {
    macro_rules! mapping {
        (pub const $name:ident = $file:literal) => {
            pub const $name: &str = include_str!(concat!(
                "../assets/UnicodePublic/MAPPINGS/",
                $file
            ));
        };
    }

    mapping!(pub const ISO_8859_1_TXT = "ISO8859/8859-1.txt");
    mapping!(pub const ISO_8859_2_TXT = "ISO8859/8859-2.txt");
    mapping!(pub const ISO_8859_3_TXT = "ISO8859/8859-3.txt");
    mapping!(pub const ISO_8859_4_TXT = "ISO8859/8859-4.txt");
    mapping!(pub const ISO_8859_5_TXT = "ISO8859/8859-5.txt");
    mapping!(pub const ISO_8859_6_TXT = "ISO8859/8859-6.txt");
    mapping!(pub const ISO_8859_7_TXT = "ISO8859/8859-7.txt");
    mapping!(pub const ISO_8859_8_TXT = "ISO8859/8859-8.txt");
    mapping!(pub const ISO_8859_9_TXT = "ISO8859/8859-9.txt");
    mapping!(pub const ISO_8859_10_TXT = "ISO8859/8859-10.txt");
    mapping!(pub const ISO_8859_11_TXT = "ISO8859/8859-11.txt");
    // NOTE: ISO 8859-12 intentionally missing, as it was never standarized.
    mapping!(pub const ISO_8859_13_TXT = "ISO8859/8859-13.txt");
    mapping!(pub const ISO_8859_14_TXT = "ISO8859/8859-14.txt");
    mapping!(pub const ISO_8859_15_TXT = "ISO8859/8859-15.txt");
    mapping!(pub const ISO_8859_16_TXT = "ISO8859/8859-16.txt");
}
