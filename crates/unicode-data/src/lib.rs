//! Unicode Character Database
//!
//! Reference: [Unicode® Standard Annex #44: Unicode Character Database](https://www.unicode.org/reports/tr44/)
//!
//! # Codepoint Operations
//!
//! * **TODO** — get all codepoints in a [`GeneralCategory`]
//! * **TODO** — get all codepoints in a [`GeneralCategoryClass`]
//!
//! # Property Types
//!
//! * [`GeneralCategory`]
//! * [`GeneralCategoryClass`] — classes of [`GeneralCategory`] values

use std::collections::HashMap;

pub mod data;

//======================================
// Macros
//======================================

macro_rules! property_and_class_enums {
    (
        $(#[$property_attr:meta])*
        property = pub enum $property_name:ident { ... };

        $(#[$property_class_attr:meta])*
        property_class = pub enum $property_class_name:ident { ... };

        values = [
            $(($short:ident, $variant:ident, $description:literal)),*
        ];

        classes = [
            $(($class_short:ident, $class_variant:ident, $($class_values:ident)|*)),*
        ];
    ) => {
        //==============================
        // Property enum definition
        //==============================

        #[allow(non_camel_case_types)]
        $(#[$property_attr])*
        #[doc = ""]
        #[doc = concat!("Classes: [`", stringify!($property_class_name), "`]")]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum $property_name {
            $(
                #[doc = concat!($description, " (\"", stringify!($short), "\")")]
                $variant
            ),*
        }

        impl $property_name {
            /// Get the variants of this enum.
            pub const fn variants() -> &'static [Self] {
                return &[
                    $(Self::$variant),*
                ];
            }

            pub const fn name(&self) -> &'static str {
                match self {
                    $(Self :: $variant => stringify!($variant)),*
                }
            }

            /// Get the short name of this property value.
            ///
            /// This is the name that will appear to encode this property in
            /// [UnicodeData.txt](https://www.unicode.org/reports/tr44/#UnicodeData.txt).
            pub const fn short(&self) -> &'static str {
                match self {
                    $(Self :: $variant => stringify!($short)),*
                }
            }

            /// Get the description of this property value.
            pub const fn description(&self) -> &'static str {
                match self {
                    $(Self :: $variant => $description),*
                }
            }

            /// Parse this property value from its short representation.
            ///
            /// Supported values include:
            $(
                #[doc = concat!(
                    "* `",
                    stringify!($short),
                    "` => [`Self::",
                    stringify!($variant),
                    "`]"
                )]
            )*
            pub fn from_short_str(str: &str) -> Option<Self> {
                let variant = match str {
                    $(stringify!($short) => Self::$variant),* ,
                    _ => return None,
                };
                Some(variant)
            }

            pub fn class_values(class: GeneralCategoryClass) -> Vec<Self> {
                let mut values = Vec::new();

                $(
                    if Self::$variant.classes().contains(&class) {
                        values.push(Self::$variant)
                    }
                )*

                values
            }

            // TODO:
            //  Should be `const`, but cannot be because it currently
            //  relies on slice::contains.
            pub fn classes(&self) -> Vec<$property_class_name> {
                let mut classes = Vec::new();

                // FIXME:
                //  This is much more inefficient than it should be. This
                //  should be a simple match statement but that's hard to do
                //  with the current structure of the input arguments.
                $(
                    if [$(stringify!($class_values)),*].contains(&self.short()) {
                        classes.push($property_class_name :: $class_variant);
                    }
                )*

                classes
                // $(
                //     $class_values => $property_class_name :: $class_variant
                // ),*
            }

            pub fn most_general_class(&self) -> $property_class_name {
                return self.classes()
                    .last()
                    .copied()
                    .expect("expected property value classes() to be non-empty");
            }
        }

        //==============================
        // Property class enum definition
        //==============================

        #[allow(non_camel_case_types)]
        $(#[$property_class_attr])*
        #[doc = concat!("Classes of [`", stringify!($property_name), "`] property values.")]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum $property_class_name {
            $(
                $class_variant
            ),*
        }

        impl $property_class_name {
            pub const fn variants() -> &'static [Self] {
                return &[
                    $(Self::$class_variant),*
                ];
            }

            pub fn name(&self) -> &'static str {
                match self {
                    $(Self :: $class_variant => stringify!($class_variant)),*
                }
            }

            pub fn short(&self) -> &'static str {
                match self {
                    $(Self :: $class_variant => stringify!($class_short)),*
                }
            }
        }
    }
}

//======================================
// Data
//======================================

macro_rules! generate_mapped_character_set_enum {
    (
        $( #[$attr:meta] )*
        pub enum $name:ident => [
            $( $variant:ident = $mapping_file:ident ),*
        ];
    ) => {
        $( #[$attr] )*
        #[allow(non_camel_case_types)]
        pub enum $name {
            $($variant),*
        }

        impl $name {
            /// Get the variants of this enum.
            pub const fn variants() -> &'static [Self] {
                return &[$(Self::$variant),*]
            }

            pub const fn variant_name(&self) -> &'static str {
                match self {
                    $( Self::$variant => stringify!($variant), )*
                }
            }

            /// Construct a [`MappedCharacterSet`] from the character set
            /// variant name.
            ///
            /// # Examples
            /// ```
            /// # use unicode_data::MappedCharacterSet;
            ///
            /// assert_eq!(
            ///     MappedCharacterSet::from_variant_name("ISO_8859_7").unwrap(),
            ///     MappedCharacterSet::ISO_8859_7
            /// );
            /// ```
            pub fn from_variant_name(string: &str) -> Option<Self> {
                let variant = match string {
                    $( stringify!($variant) => Self::$variant, )*
                    _ => return None,
                };
                Some(variant)
            }

            /// Raw contents of the <https://unicode.org/Public/MAPPINGS/> entry
            /// file for the given character set.
            #[rustfmt::skip]
            pub const fn raw_mapping_file_contents(&self) -> &'static str {
                match self {
                    $(
                        Self::$variant => $crate::data::mappings::$mapping_file,
                    )*
                }
            }
        }
    };
}

generate_mapped_character_set_enum!(
    /// Character sets that have official mappings into Unicode.
    ///
    /// Reference: <https://unicode.org/Public/MAPPINGS/>
    ///
    /// **NOTE:** ISO 8859-12 is intentionally missing as it was never standardized.
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum MappedCharacterSet => [
        //
        // ISO 8859
        //
        ISO_8859_1 = ISO_8859_1_TXT,
        ISO_8859_2 = ISO_8859_2_TXT,
        ISO_8859_3 = ISO_8859_3_TXT,
        ISO_8859_4 = ISO_8859_4_TXT,
        ISO_8859_5 = ISO_8859_5_TXT,
        ISO_8859_6 = ISO_8859_6_TXT,
        ISO_8859_7 = ISO_8859_7_TXT,
        ISO_8859_8 = ISO_8859_8_TXT,
        ISO_8859_9 = ISO_8859_9_TXT,
        ISO_8859_10 = ISO_8859_10_TXT,
        ISO_8859_11 = ISO_8859_11_TXT,
        ISO_8859_13 = ISO_8859_13_TXT,
        ISO_8859_14 = ISO_8859_14_TXT,
        ISO_8859_15 = ISO_8859_15_TXT,
        ISO_8859_16 = ISO_8859_16_TXT

        //
        // Vendor-Specific
        //
        // Apple_Arabic,
        // Apple_Celtic,
        // Apple_CentralEuropean,
        // Apple_ChineseSimplified,
        // Apple_ChineseTraditional,
        // etc.
    ];
);

impl MappedCharacterSet {
    pub fn to_unicode_codepoints(&self) -> HashMap<u32, char> {
        let raw_text = self.raw_mapping_file_contents();

        let data = raw_text
            .lines()
            .flat_map(|line: &str| -> Option<(u32, char)> {
                parse_mapping_line(*self, line)
            })
            .collect();

        return data;
    }
}

fn parse_mapping_line(
    char_set: MappedCharacterSet,
    line: &str,
) -> Option<(u32, char)> {
    if line.starts_with('#') {
        return None;
    }

    if line == "" {
        return None;
    }

    let fields: Vec<&str> = line.split('\t').collect();

    let fields: [&str; 4] = match fields.try_into() {
        Ok(fields) => fields,
        Err(fields) => panic!(
            "unexpected number of fields in MAPPINGS file for {char_set:?}: {}",
            fields.len()
        ),
    };

    let [external, unicode, comment_char, _comment_name] = fields;

    let external = external
        .strip_prefix("0x")
        .expect("expected field prefix to be '0x'");
    let unicode = unicode
        .strip_prefix("0x")
        .expect("expected field prefix to be '0x'");
    assert_eq!(comment_char, "#");

    let external = u32::from_str_radix(external, 16)
        .expect("expected field to be hex number");
    let unicode = u32::from_str_radix(unicode, 16)
        .expect("expected field to be hex number");

    let unicode = char::from_u32(unicode)
        .expect("expected field to be valid Unicode codepoint");

    Some((external, unicode))
}

#[test]
fn test_mapped_character_set() {
    let iso_8859_7 = MappedCharacterSet::ISO_8859_7.to_unicode_codepoints();

    assert_eq!(
        u32::from(iso_8859_7[&0xE1]),
        0x03B1 // 'α'
    );
}

//======================================
// Properties
//======================================

property_and_class_enums!(
    /// Values of the unicode
    /// [General_Category](https://www.unicode.org/reports/tr44/#General_Category)
    /// property.
    ///
    /// Reference: <https://www.unicode.org/reports/tr44/#General_Category_Values>
    ///
    /// # Examples
    ///
    /// Get the short name of a given category:
    ///
    /// ```
    /// # use unicode_data::{GeneralCategory, GeneralCategoryClass};
    /// assert_eq!(GeneralCategory::Uppercase_Letter.short(), "Lu");
    /// assert_eq!(GeneralCategory::Other_Letter.short(), "Lo");
    /// assert_eq!(GeneralCategory::Math_Symbol.short(), "Sm");
    /// assert_eq!(GeneralCategory::Currency_Symbol.short(), "Sc");
    /// ```
    ///
    /// Get the classes of a category value:
    ///
    /// ```
    /// # use unicode_data::{GeneralCategory, GeneralCategoryClass};
    /// assert_eq!(
    ///     GeneralCategory::Spacing_Mark.classes(),
    ///     &[GeneralCategoryClass::Mark]
    /// );
    ///
    /// assert_eq!(
    ///     GeneralCategory::Uppercase_Letter.classes(),
    ///     &[
    ///         GeneralCategoryClass::Cased_Letter,
    ///         GeneralCategoryClass::Letter
    ///     ]
    /// );
    /// ```
    ///
    /// Get the category values in a category class:
    ///
    /// ```
    /// # use unicode_data::{GeneralCategory, GeneralCategoryClass};
    /// assert_eq!(
    ///     GeneralCategory::class_values(GeneralCategoryClass::Number),
    ///     &[
    ///         GeneralCategory::Decimal_Number,
    ///         GeneralCategory::Letter_Number,
    ///         GeneralCategory::Other_Number
    ///     ]
    /// );
    /// ```
    ///
    /// # Operations
    ///
    /// * [`Self::variants()`] — get all category values
    /// * [`Self::from_short_str()`] — parse property value from short name ("")
    /// * [`GeneralCategoryClass::variants()`] — get all category classes (Letter, Mark, etc.)
    /// * [`Self::class_values()`] — get all category values belonging to a given class
    /// * [`value.classes()`][Self::classes] — get all classes to which a category belongs
    property = pub enum GeneralCategory { ... };

    property_class = pub enum GeneralCategoryClass { ... };

    values = [
        (Lu, Uppercase_Letter, "an uppercase letter"),
        (Ll, Lowercase_Letter, "a lowercase letter"),
        (Lt, Titlecase_Letter, "a digraph encoded as a single character, with first part uppercase"),
        (Lm, Modifier_Letter, "a modifier letter"),
        (Lo, Other_Letter, "other letters, including syllables and ideographs"),
        (Mn, Nonspacing_Mark, "a nonspacing combining mark (zero advance width)"),
        (Mc, Spacing_Mark, "a spacing combining mark (positive advance width)"),
        (Me, Enclosing_Mark, "an enclosing combining mark"),
        (Nd, Decimal_Number, "a decimal digit"),
        (Nl, Letter_Number, "a letterlike numeric character"),
        (No, Other_Number, "a numeric character of other type"),
        (Pc, Connector_Punctuation, "a connecting punctuation mark, like a tie"),
        (Pd, Dash_Punctuation, "a dash or hyphen punctuation mark"),
        (Ps, Open_Punctuation, "an opening punctuation mark (of a pair)"),
        (Pe, Close_Punctuation, "a closing punctuation mark (of a pair)"),
        (Pi, Initial_Punctuation, "an initial quotation mark"),
        (Pf, Final_Punctuation, "a final quotation mark"),
        (Po, Other_Punctuation, "a punctuation mark of other type"),
        (Sm, Math_Symbol, "a symbol of mathematical use"),
        (Sc, Currency_Symbol, "a currency sign"),
        (Sk, Modifier_Symbol, "a non-letterlike modifier symbol"),
        (So, Other_Symbol, "a symbol of other type"),
        (Zs, Space_Separator, "a space character (of various non-zero widths)"),
        (Zl, Line_Separator, "U+2028 LINE SEPARATOR only"),
        (Zp, Paragraph_Separator, "U+2029 PARAGRAPH SEPARATOR only"),
        (Cc, Control, "a C0 or C1 control code"),
        (Cf, Format, "a format control character"),
        (Cs, Surrogate, "a surrogate code point"),
        (Co, Private_Use, "a private-use character"),
        (Cn, Unassigned, "a reserved unassigned code point or a noncharacter")
    ];

    classes = [
        (LC, Cased_Letter, Lu | Ll | Lt),
        (L, Letter, Lu | Ll | Lt | Lm | Lo),
        (M, Mark, Mn | Mc | Me),
        (N, Number, Nd | Nl | No),
        (P, Punctuation, Pc | Pd | Ps | Pe | Pi | Pf | Po),
        (S, Symbol, Sm | Sc | Sk | So),
        (Z, Separator, Zs | Zl | Zp),
        (C, Other, Cc | Cf | Cs | Co | Cn)
    ];
);

//==========================================================
// Tests
//==========================================================

#[test]
fn test_general_category_property_classes() {
    assert_eq!(
        GeneralCategory::Letter_Number.classes(),
        &[GeneralCategoryClass::Number]
    );

    assert_eq!(
        GeneralCategory::Uppercase_Letter.classes(),
        &[
            GeneralCategoryClass::Cased_Letter,
            GeneralCategoryClass::Letter
        ]
    );

    //----------------------------------
    // most_general_class()
    //----------------------------------

    assert_eq!(
        GeneralCategory::Uppercase_Letter.most_general_class(),
        GeneralCategoryClass::Letter
    );
}
