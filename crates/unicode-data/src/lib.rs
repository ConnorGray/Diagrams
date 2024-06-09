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
