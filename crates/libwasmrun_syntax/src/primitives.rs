use crate::{io, Deserialize, Error};

const PRIMITIVES_BUFFER_LENGTH: usize = 1024;

impl Deserialize for u8 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buf = [0u8; 1];
        reader.read(&mut buf)?;
        Ok(buf[0])
    }
}

/// Unsigned variable-length integer, limited to 32 bits, represented by at most 5 bytes that may
/// contain padding 0x80 bytes.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarUint32(u32);

impl From<VarUint32> for usize {
    fn from(var: VarUint32) -> usize {
        var.0 as usize
    }
}

impl From<VarUint32> for u32 {
    fn from(var: VarUint32) -> u32 {
        var.0
    }
}

impl From<u32> for VarUint32 {
    fn from(i: u32) -> VarUint32 {
        VarUint32(i)
    }
}

impl From<usize> for VarUint32 {
    fn from(i: usize) -> VarUint32 {
        assert!(i <= u32::MAX as usize);
        VarUint32(i as u32)
    }
}

impl Deserialize for VarUint32 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut res = 0;
        let mut shift = 0;
        let mut buf = [0u8; 1];
        loop {
            if shift > 31 {
                return Err(Error::InvalidVarUint32);
            }

            reader.read(&mut buf)?;
            let b = buf[0] as u32;
            res |= (b & 0x7f)
                .checked_shl(shift)
                .ok_or(Error::InvalidVarUint32)?;
            shift += 7;
            if (b >> 7) == 0 {
                if shift >= 32 && (b as u8).leading_zeros() < 4 {
                    return Err(Error::InvalidVarInt32);
                }
                break;
            }
        }
        Ok(VarUint32(res))
    }
}

/// Unsigned variable-length integer, limited to 64 bits, represented by at most 9 bytes that may
/// contain padding 0x80 bytes.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarUint64(u64);

impl From<VarUint64> for u64 {
    fn from(var: VarUint64) -> u64 {
        var.0
    }
}

impl Deserialize for VarUint64 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut res = 0;
        let mut shift = 0;
        let mut buf = [0u8; 1];
        loop {
            if shift > 63 {
                return Err(Error::InvalidVarUint64);
            }

            reader.read(&mut buf)?;
            let b = buf[0] as u64;
            res |= (b & 0x7f)
                .checked_shl(shift)
                .ok_or(Error::InvalidVarUint64)?;
            shift += 7;
            if (b >> 7) == 0 {
                if shift >= 64 && (b as u8).leading_zeros() < 7 {
                    return Err(Error::InvalidVarInt64);
                }
                break;
            }
        }
        Ok(VarUint64(res))
    }
}

impl From<u64> for VarUint64 {
    fn from(u: u64) -> VarUint64 {
        VarUint64(u)
    }
}

/// 7-bit unsigned integer, encoded in LEB128 (always 1 byte length).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarUint7(u8);

impl From<VarUint7> for u8 {
    fn from(v: VarUint7) -> u8 {
        v.0
    }
}

impl From<u8> for VarUint7 {
    fn from(v: u8) -> Self {
        VarUint7(v)
    }
}

impl Deserialize for VarUint7 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buf = [0u8; 1];
        reader.read(&mut buf)?;
        Ok(VarUint7(buf[0]))
    }
}

/// 32-bit signed integer, encoded in LEB128 (can be 1-5 bytes length).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarInt32(i32);

impl From<VarInt32> for i32 {
    fn from(v: VarInt32) -> i32 {
        v.0
    }
}

impl From<i32> for VarInt32 {
    fn from(v: i32) -> VarInt32 {
        VarInt32(v)
    }
}

impl Deserialize for VarInt32 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut res = 0;
        let mut shift = 0;
        let mut buf = [0u8; 1];
        loop {
            if shift > 31 {
                return Err(Error::InvalidVarInt32);
            }
            reader.read(&mut buf)?;
            let b = buf[0];

            res |= ((b & 0b0111_1111) as i32)
                .checked_shl(shift)
                .ok_or(Error::InvalidVarInt32)?;

            shift += 7;
            if b >> 7 == 0 {
                if shift < 32 && b & 0b0100_0000 == 0b0100_0000 {
                    res |= (1i32 << shift).wrapping_neg();
                } else if shift >= 32 && b & 0b0100_0000 == 0b0100_0000 {
                    if (!(b | 0b1000_0000)).leading_zeros() < 5 {
                        return Err(Error::InvalidVarInt32);
                    }
                } else if shift >= 32 && b & 0b0100_0000 == 0 && b.leading_zeros() < 5 {
                    return Err(Error::InvalidVarInt32);
                }
                break;
            }
        }
        Ok(VarInt32(res))
    }
}

/// 64-bit signed integer, encoded in LEB128 (can be 1-9 bytes length).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarInt64(i64);

impl From<VarInt64> for i64 {
    fn from(v: VarInt64) -> i64 {
        v.0
    }
}

impl From<i64> for VarInt64 {
    fn from(v: i64) -> VarInt64 {
        VarInt64(v)
    }
}

impl Deserialize for VarInt64 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut res = 0i64;
        let mut shift = 0;
        let mut buf = [0u8; 1];

        loop {
            if shift > 63 {
                return Err(Error::InvalidVarInt64);
            }
            reader.read(&mut buf)?;
            let b = buf[0];

            res |= ((b & 0b0111_1111) as i64)
                .checked_shl(shift)
                .ok_or(Error::InvalidVarInt64)?;

            shift += 7;
            if (b >> 7) == 0 {
                if shift < 64 && b & 0b0100_0000 == 0b0100_0000 {
                    res |= (1i64 << shift).wrapping_neg();
                } else if shift >= 64 && b & 0b0100_0000 == 0b0100_0000 {
                    if (b | 0b1000_0000) as i8 != -1 {
                        return Err(Error::InvalidVarInt64);
                    }
                } else if shift >= 64 && b != 0 {
                    return Err(Error::InvalidVarInt64);
                }
                break;
            }
        }
        Ok(VarInt64(res))
    }
}

/// 32-bit unsigned integer, encoded in little endian.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Uint32(u32);

impl Deserialize for Uint32 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buf = [0u8; 4];
        reader.read(&mut buf)?;
        // todo check range
        Ok(u32::from_le_bytes(buf).into())
    }
}

impl From<Uint32> for u32 {
    fn from(var: Uint32) -> u32 {
        var.0
    }
}

impl From<u32> for Uint32 {
    fn from(u: u32) -> Self {
        Uint32(u)
    }
}

/// 64-bit unsigned integer, encoded in little endian.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Uint64(u64);

impl Deserialize for Uint64 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buf = [0u8; 8];
        reader.read(&mut buf)?;
        // todo check range
        Ok(u64::from_le_bytes(buf).into())
    }
}

impl From<u64> for Uint64 {
    fn from(u: u64) -> Self {
        Uint64(u)
    }
}

impl From<Uint64> for u64 {
    fn from(var: Uint64) -> u64 {
        var.0
    }
}

/// VarUint1, 1-bit value (0/1).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarUint1(bool);

impl From<VarUint1> for bool {
    fn from(v: VarUint1) -> bool {
        v.0
    }
}

impl From<bool> for VarUint1 {
    fn from(b: bool) -> Self {
        VarUint1(b)
    }
}

impl Deserialize for VarUint1 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buf = [0u8; 1];
        reader.read(&mut buf)?;
        match buf[0] {
            0 => Ok(VarUint1(false)),
            1 => Ok(VarUint1(true)),
            v => Err(Error::InvalidVarUint1(v)),
        }
    }
}

impl Deserialize for String {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let length = u32::from(VarUint32::deserialize(reader)?) as usize;
        String::from_utf8(io::buffered_read::<R, PRIMITIVES_BUFFER_LENGTH>(
            length, reader,
        )?)
        .map_err(|_| Error::NonUtf8String)
    }
}

/// A list of things, serialized with a `VarUint32` length prefix.
#[derive(Debug, Clone)]
pub struct CountedList<T>(Vec<T>);

impl<T> CountedList<T> {
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T: Deserialize> Deserialize for CountedList<T> {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        CountedList::<T>::deserialize_with(reader, T::deserialize)
    }
}

impl<T> CountedList<T> {
    pub fn deserialize_with<R, F>(reader: &mut R, deserialize_element: F) -> Result<Self, Error>
    where
        R: io::Read,
        F: Fn(&mut R) -> Result<T, Error>,
    {
        let count: usize = VarUint32::deserialize(reader)?.into();
        let mut result = Vec::new();
        for _ in 0..count {
            result.push(deserialize_element(reader)?);
        }
        Ok(CountedList(result))
    }
}

#[cfg(test)]
mod tests {
    use crate::{deserialize_buffer, CountedList, Error, VarInt32, VarInt64, VarUint32, VarUint64};

    fn varuint32_de_test(dt: Vec<u8>, expected: u32) {
        let val: VarUint32 = deserialize_buffer(&dt).expect("buf to be serialized");
        assert_eq!(expected, val.into());
    }

    fn varuint32_serde_test(dt: Vec<u8>, val: u32) {
        varuint32_de_test(dt.clone(), val);
    }

    fn varint32_de_test(dt: Vec<u8>, expected: i32) {
        let val: VarInt32 = deserialize_buffer(&dt).expect("buf to be serialized");
        assert_eq!(expected, val.into());
    }

    fn varint32_serde_test(dt: Vec<u8>, val: i32) {
        varint32_de_test(dt.clone(), val);
    }

    fn varuint64_de_test(dt: Vec<u8>, expected: u64) {
        let val: VarUint64 = deserialize_buffer(&dt).expect("buf to be serialized");
        assert_eq!(expected, val.into());
    }

    fn varuint64_serde_test(dt: Vec<u8>, val: u64) {
        varuint64_de_test(dt.clone(), val);
    }

    fn varint64_de_test(dt: Vec<u8>, expected: i64) {
        let val: VarInt64 = deserialize_buffer(&dt).expect("buf to be serialized");
        assert_eq!(expected, val.into());
    }

    fn varint64_serde_test(dt: Vec<u8>, val: i64) {
        varint64_de_test(dt.clone(), val);
    }

    #[test]
    fn varuint32_0() {
        varuint32_serde_test(vec![0u8; 1], 0);
    }

    #[test]
    fn varuint32_1() {
        varuint32_serde_test(vec![1u8; 1], 1);
    }

    #[test]
    fn varuint32_135() {
        varuint32_serde_test(vec![135u8, 0x01], 135);
    }

    #[test]
    fn varuint32_8192() {
        varuint32_serde_test(vec![0x80, 0x40], 8192);
    }

    #[test]
    fn varint32_8192() {
        varint32_serde_test(vec![0x80, 0xc0, 0x00], 8192);
    }

    #[test]
    fn varint32_neg_8192() {
        varint32_serde_test(vec![0x80, 0x40], -8192);
    }

    #[test]
    fn varuint64_0() {
        varuint64_serde_test(vec![0u8; 1], 0);
    }

    #[test]
    fn varuint64_1() {
        varuint64_serde_test(vec![1u8; 1], 1);
    }

    #[test]
    fn varuint64_135() {
        varuint64_serde_test(vec![135u8, 0x01], 135);
    }

    #[test]
    fn varuint64_8192() {
        varuint64_serde_test(vec![0x80, 0x40], 8192);
    }

    #[test]
    fn varint64_8192() {
        varint64_serde_test(vec![0x80, 0xc0, 0x00], 8192);
    }

    #[test]
    fn varint64_neg_8192() {
        varint64_serde_test(vec![0x80, 0x40], -8192);
    }

    #[test]
    fn varint64_min() {
        varint64_serde_test(
            vec![0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7f],
            -9223372036854775808,
        );
    }

    #[test]
    fn varint64_bad_extended() {
        let res = deserialize_buffer::<VarInt64>(
            &[0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x6f][..],
        );
        assert!(res.is_err());
    }

    #[test]
    fn varint32_bad_extended() {
        let res = deserialize_buffer::<VarInt32>(&[0x80, 0x80, 0x80, 0x80, 0x6f][..]);
        assert!(res.is_err());
    }

    #[test]
    fn varint32_bad_extended2() {
        let res = deserialize_buffer::<VarInt32>(&[0x80, 0x80, 0x80, 0x80, 0x41][..]);
        assert!(res.is_err());
    }

    #[test]
    fn varint64_max() {
        varint64_serde_test(
            vec![0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00],
            9223372036854775807,
        );
    }

    #[test]
    fn varint64_too_long() {
        assert!(deserialize_buffer::<VarInt64>(
            &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00][..],
        )
        .is_err());
    }

    #[test]
    fn varint32_too_long() {
        assert!(
            deserialize_buffer::<VarInt32>(&[0xff, 0xff, 0xff, 0xff, 0xff, 0x00][..],).is_err()
        );
    }

    #[test]
    fn varuint64_too_long() {
        assert!(deserialize_buffer::<VarUint64>(
            &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00][..],
        )
        .is_err());
    }

    #[test]
    fn varuint32_too_long() {
        assert!(
            deserialize_buffer::<VarUint32>(&[0xff, 0xff, 0xff, 0xff, 0xff, 0x00][..],).is_err()
        );
    }

    #[test]
    fn varuint32_too_long_trailing() {
        assert!(deserialize_buffer::<VarUint32>(&[0xff, 0xff, 0xff, 0xff, 0x7f][..],).is_err());
    }

    #[test]
    fn varuint64_too_long_trailing() {
        assert!(deserialize_buffer::<VarUint64>(
            &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x04][..],
        )
        .is_err());
    }

    #[test]
    fn varint32_min() {
        varint32_serde_test(vec![0x80, 0x80, 0x80, 0x80, 0x78], -2147483648);
    }

    #[test]
    fn varuint32_too_long_nulled() {
        match deserialize_buffer::<VarUint32>(&[
            0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x78,
        ]) {
            Err(Error::InvalidVarUint32) => {}
            _ => panic!("Should be invalid VarUint32"),
        }
    }

    #[test]
    fn varint32_max() {
        varint32_serde_test(vec![0xff, 0xff, 0xff, 0xff, 0x07], 2147483647);
    }

    #[test]
    fn counted_list() {
        #[rustfmt::skip]
        let payload = [
            // 5 with padding:
            0b1000_0101, 0b1000_0000, 0b1000_0000, 0b1000_0000, 0,
            // Contents:
            0x01, 0x7d, 0x05, 0x07, 0x09,
        ];
        let list: CountedList<u8> = deserialize_buffer(&payload).unwrap();
        let vars = list.into_inner();
        assert_eq!(vars, vec![1, 125, 5, 7, 9]);
    }
}
