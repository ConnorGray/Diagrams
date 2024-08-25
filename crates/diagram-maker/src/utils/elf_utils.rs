//! Utilities for working with ELF data from the [`goblin`] library.
//!
//! A low-priority goal is to upstream these types into [`goblin`].
#![allow(dead_code)]

//======================================
// Constants
//======================================

use goblin::elf::section_header::{
    SHT_DYNAMIC, SHT_DYNSYM, SHT_HASH, SHT_HIPROC, SHT_HIUSER, SHT_LOPROC,
    SHT_LOUSER, SHT_NOBITS, SHT_NOTE, SHT_NULL, SHT_PROGBITS, SHT_REL,
    SHT_RELA, SHT_SHLIB, SHT_STRTAB, SHT_SYMTAB,
};

//======================================
// Types
//======================================

/// Parses from [`goblin::elf::SectionHeader::sh_type`].
///
/// # Examples
///
/// Parse a [`SectionHeaderType`] from a raw value:
///
/// ```
/// # use diagram_maker::utils::elf_utils::SectionHeaderType;
///
/// assert_equal!(SectionHeaderType::try_from(2), SectionHeaderType::SymTab);
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SectionHeaderType {
    /// [`SHT_NULL`]
    Null,
    /// [`SHT_PROGBITS`]
    ProgBits,
    /// [`SHT_SYMTAB`]
    SymTab,
    /// [`SHT_STRTAB`]
    StrTab,
    /// [`SHT_RELA`]
    Rela,
    /// [`SHT_HASH`]
    Hash,
    /// [`SHT_DYNAMIC`]
    Dynamic,
    /// [`SHT_NOTE`]
    Note,
    /// [`SHT_NOBITS`]
    NoBits,
    /// [`SHT_REL`]
    Rel,
    /// [`SHT_SHLIB`]
    ShLib,
    /// [`SHT_DYNSYM`]
    DynSym,
    /// Value in the range [`SHT_LOPROC`] – [`SHT_HIPROC`].
    Proc(u32),
    /// Value in the range [`SHT_LOUSER`] – [`SHT_HIUSER`].
    User(u32),
}

//======================================
// Impls
//======================================

impl SectionHeaderType {
    /// Convert this value to a u32 suitable for storage in an ELF section
    /// header [`sh_type`][goblin::elf::SectionHeader::sh_type] field.
    pub fn to_u32(&self) -> u32 {
        match self {
            SectionHeaderType::Null => todo!(),
            SectionHeaderType::ProgBits => todo!(),
            SectionHeaderType::SymTab => todo!(),
            SectionHeaderType::StrTab => todo!(),
            SectionHeaderType::Rela => todo!(),
            SectionHeaderType::Hash => todo!(),
            SectionHeaderType::Dynamic => todo!(),
            SectionHeaderType::Note => todo!(),
            SectionHeaderType::NoBits => todo!(),
            SectionHeaderType::Rel => todo!(),
            SectionHeaderType::ShLib => todo!(),
            SectionHeaderType::DynSym => todo!(),
            SectionHeaderType::Proc(_) => todo!(),
            SectionHeaderType::User(_) => todo!(),
        }
    }

    /// Returns a short human-readable descriptive identifier of this type.
    pub fn as_str(&self) -> &str {
        match self {
            SectionHeaderType::Null => "Null",
            SectionHeaderType::ProgBits => "ProgBits",
            SectionHeaderType::SymTab => "SymTab",
            SectionHeaderType::StrTab => "StrTab",
            SectionHeaderType::Rela => "Rela",
            SectionHeaderType::Hash => "Hash",
            SectionHeaderType::Dynamic => "Dynamic",
            SectionHeaderType::Note => "Note",
            SectionHeaderType::NoBits => "NoBits",
            SectionHeaderType::Rel => "Rel",
            SectionHeaderType::ShLib => "ShLib",
            SectionHeaderType::DynSym => "DynSym",
            SectionHeaderType::Proc(_) => "Proc(..)",
            SectionHeaderType::User(_) => "User(..)",
        }
    }
}

//======================================
// Conversion Impls
//======================================

impl TryFrom<u32> for SectionHeaderType {
    type Error = u32;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let header_type = match value {
            SHT_NULL => SectionHeaderType::Null,
            SHT_PROGBITS => SectionHeaderType::ProgBits,
            SHT_SYMTAB => SectionHeaderType::SymTab,
            SHT_STRTAB => SectionHeaderType::StrTab,
            SHT_RELA => SectionHeaderType::Rela,
            SHT_HASH => SectionHeaderType::Hash,
            SHT_DYNAMIC => SectionHeaderType::Dynamic,
            SHT_NOTE => SectionHeaderType::Note,
            SHT_NOBITS => SectionHeaderType::NoBits,
            SHT_REL => SectionHeaderType::Rel,
            SHT_SHLIB => SectionHeaderType::ShLib,
            SHT_DYNSYM => SectionHeaderType::DynSym,
            SHT_LOPROC..=SHT_HIPROC => SectionHeaderType::Proc(value),
            SHT_LOUSER..=SHT_HIUSER => SectionHeaderType::User(value),
            other => return Err(other),
        };

        Ok(header_type)
    }
}
