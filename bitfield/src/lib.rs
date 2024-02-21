// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::bitfield;
use bitfield_impl::impl_specifier;
pub use bitfield_impl::BitfieldSpecifier;

// TODO other things
use seq::seq;

pub trait Specifier {
    type ParamType;
    const BITS: usize;

    unsafe fn read<const BIT_OFFSET: usize>(arr_ptr: *const u8) -> Self::ParamType;

    unsafe fn write<const BIT_OFFSET: usize>(arr_ptr: *mut u8, val: Self::ParamType);
}

pub mod convert {

    #[macro_export]
    macro_rules! utype {
        ($n:expr) => {
            <[(); $n] as $crate::convert::UType>::Type
        };
    }

    pub trait UType {
        type Type;
    }

    impl UType for [(); 1] {
        type Type = u8;
    }
    impl UType for [(); 2] {
        type Type = u16;
    }
    impl UType for [(); 4] {
        type Type = u8;
    }
    impl UType for [(); 8] {
        type Type = u64;
    }
}

pub mod checks {

    pub trait TotalSizeIsMultipleOfEightBits {}
    pub enum ZeroMod8 {}
    pub enum OneMod8 {}
    pub enum TwoMod8 {}
    pub enum ThreeMod8 {}
    pub enum FourMod8 {}
    pub enum FiveMod8 {}
    pub enum SixMod8 {}
    pub enum SevenMod8 {}

    impl TotalSizeIsMultipleOfEightBits for ZeroMod8 {}

    pub trait DiscriminantInRange {}
    pub enum False {}
    pub enum True {}
    impl DiscriminantInRange for True {}
}

pub unsafe fn read<const BIT_OFFSET: usize, const BITS: usize>(arr_ptr: *const u8) -> u64 {
    let read_start = BIT_OFFSET / 8;
    let read_end = (BIT_OFFSET + BITS - 1) / 8;
    let mut result = 0u64;
    unsafe {
        let base = arr_ptr.add(read_start);
        ::core::ptr::copy_nonoverlapping(
            base,
            <*mut _>::from(&mut result).cast(),
            read_end - read_start + 1,
        );
    }
    let mask = u64::MAX >> (64 - BITS);
    (result >> (BIT_OFFSET % 8)) & mask
}

pub unsafe fn write<const BIT_OFFSET: usize, const BITS: usize>(arr_ptr: *mut u8, val: u64) {
    let read_start = BIT_OFFSET / 8;
    let read_end = (BIT_OFFSET + BITS - 1) / 8;
    let mask = u64::MAX >> (64 - BITS);
    let mut result = 0u64;
    let base = arr_ptr.add(read_start);
    unsafe {
        ::core::ptr::copy_nonoverlapping(
            base,
            <*mut _>::from(&mut result).cast(),
            read_end - read_start + 1,
        )
    }
    // set bits to zeros
    result &= !(mask << (BIT_OFFSET % 8));
    // truncate val first and write it to right place
    result |= (val & mask) << (BIT_OFFSET % 8);
    unsafe {
        ::core::ptr::copy_nonoverlapping(
            <*const _>::from(&result).cast(),
            base,
            read_end - read_start + 1,
        )
    }
}

seq!(N in 1..=64 {
    pub enum B~N {}
    impl_specifier!(B~N, N);
});

impl Specifier for bool {
    type ParamType = bool;

    const BITS: usize = 1;

    unsafe fn read<const BIT_OFFSET: usize>(arr_ptr: *const u8) -> Self::ParamType {
        match read::<BIT_OFFSET, 1>(arr_ptr) {
            0 => false,
            _ => true,
        }
    }

    unsafe fn write<const BIT_OFFSET: usize>(arr_ptr: *mut u8, val: Self::ParamType) {
        write::<BIT_OFFSET, 1>(arr_ptr, val as u64);
    }
}
