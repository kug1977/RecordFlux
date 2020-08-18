from typing import List, Sequence, Union


class Bitstring:
    def __init__(self, data: Union[bytes, int, List[int]] = bytes(), length: int = 0):
        self._length = length
        if isinstance(data, (bytes, list)):
            self._data = list(data)
            self._length = length if length else len(self._data) * 8
        elif isinstance(data, int):
            if self._length % 8 == 0:
                self._data = list(data.to_bytes(self._length // 8, "big"))
            else:
                shift_r = self._length % 8
                shift_l = 8 - shift_r
                data_r = list(data.to_bytes((self._length + shift_l) // 8, "big"))
                self._data = [
                    (data_r[x] << shift_l) & 255
                    if x == len(data_r) - 1
                    else ((data_r[x] << shift_l) + (data_r[x + 1] >> shift_r)) & 255
                    for x in range(len(data_r))
                ]
        assert all(x in range(0, 256) for x in self._data)

    def append(self, other: Union["Bitstring", bytes]) -> None:
        if isinstance(other, bytes):
            self.append(Bitstring(other, len(other) * 8))
            return
        if self._length % 8 == 0:
            self._data += other.raw
            self._length += len(other)
        else:
            shift_r = self._length % 8
            shift_l = 8 - shift_r
            self._length += len(other)
            remain = len(other)
            for e in other.raw:
                self._data[-1] += e >> shift_r
                remain -= shift_l
                if not remain:
                    return
                remain -= shift_r
                self._data.append((e << shift_l) & 255)

    @property
    def raw(self) -> Sequence[int]:
        return self._data

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._data}, {self._length})"

    def __add__(self, other: Union["Bitstring", bytes]) -> "Bitstring":
        b = Bitstring(self._data, self._length)
        b.append(other)
        return b

    def __iadd__(self, other: Union["Bitstring", bytes]) -> "Bitstring":
        self.append(other)
        return self

    def __int__(self) -> int:
        if self._length % 8 == 0:
            return int.from_bytes(bytes(self._data), "big")
        shift_l = self._length % 8
        shift_r = 8 - shift_l
        # right align all bytes to convert them as big endian integer
        return int.from_bytes(
            bytes(
                [
                    self._data[x] >> shift_r
                    if x <= 0
                    else ((self._data[x] >> shift_r) + (self._data[x - 1] << shift_l)) & 255
                    for x in range(len(self._data))
                ]
            ),
            "big",
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Bitstring):
            return NotImplemented
        return self._length == other._length and self._data == other._data

    def __bytes__(self) -> bytes:
        return bytes(self._data)

    def __len__(self) -> int:
        return self._length

    def __getitem__(self, key: Union[int, slice]) -> "Bitstring":
        if isinstance(key, int):
            return Bitstring((self._data[key // 8] >> (7 - key % 8)) & 1, 1)
        bit_first = key.start if isinstance(key.start, int) else 0
        bit_last = (
            key.stop if isinstance(key.stop, int) and self._length > key.stop else self._length
        )
        if bit_first == 0 and bit_last == self._length:
            return self
        bit_length = bit_last - bit_first
        byte_first = bit_first // 8
        byte_last = bit_last // 8 + int(bit_last % 8 != 0)
        byte_length = bit_length // 8 + int(bit_length % 8 != 0)
        shift_l = bit_first % 8
        shift_r = 8 - shift_l
        mask = (255 << ((8 - bit_last % 8) % 8)) & 255
        return Bitstring(
            [
                (
                    (self._data[x] << shift_l) + (self._data[x + 1] >> shift_r)
                    & (mask if x == byte_last - 1 else 255)
                )
                if x < len(self._data) - 1
                else (self._data[x] << shift_l) & 255
                for x in range(byte_first, byte_last)
            ][:byte_length],
            bit_length,
        )

    @staticmethod
    def join(iterable: Sequence["Bitstring"]) -> "Bitstring":
        joined_bitstring = Bitstring()
        for i in iterable:
            joined_bitstring.append(i)
        return joined_bitstring
