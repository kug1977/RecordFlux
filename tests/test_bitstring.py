from rflx.pyrflx import Bitstring


def test_initialize_bytes() -> None:
    b = Bitstring(bytes([1, 2, 4]), 24)
    assert len(b) == 24
    assert bytes(b) == bytes([1, 2, 4])
    b = Bitstring(bytes([1, 2, 4]), 22)
    assert len(b) == 22
    assert bytes(b) == bytes([1, 2, 4])

    b = Bitstring([1, 2, 4], 24)
    assert len(b) == 24
    assert bytes(b) == bytes([1, 2, 4])
    b = Bitstring([1, 2, 4], 22)
    assert len(b) == 22
    assert bytes(b) == bytes([1, 2, 4])


def test_initialize_int() -> None:
    b = Bitstring(66052, 24)
    assert len(b) == 24
    assert bytes(b) == bytes([1, 2, 4])
    b = Bitstring(16513, 22)
    assert len(b) == 22
    assert bytes(b) == bytes([1, 2, 4])


def test_int() -> None:
    b = Bitstring(bytes([1, 2, 4]), 24)
    assert int(b) == 66052
    b = Bitstring(bytes([1, 2, 4]), 22)
    assert int(b) == 16513
    b = Bitstring([4, 8, 7, 254], 30)
    assert int(b) == 16908799


def test_append() -> None:
    b = Bitstring()
    assert bytes(b) == bytes()
    b.append(Bitstring(42, 8))
    assert len(b) == 8
    assert bytes(b) == bytes([42])
    b += Bitstring(15, 4)
    assert len(b) == 12
    assert bytes(b) == bytes([42, 240])
    b += Bitstring(0, 4)
    assert len(b) == 16
    assert bytes(b) == bytes([42, 240])
    b += Bitstring(63, 6)
    assert len(b) == 22
    assert bytes(b) == bytes([42, 240, 252])
    b += bytes(1)
    assert len(b) == 30
    assert bytes(b) == bytes([42, 240, 252, 0])
    b = Bitstring(1, 2)
    b.append(Bitstring(8, 14))
    assert b.raw == [64, 8]


def test_eq() -> None:
    assert Bitstring([1, 2, 4], 24) == Bitstring([1, 2, 4], 24)
    assert Bitstring([1, 2, 4], 24) == Bitstring(bytes([1, 2, 4]), 24)
    assert Bitstring([1, 2, 4], 24) != Bitstring([1, 2, 4], 22)
    assert Bitstring([1, 2, 4], 22) == Bitstring(16513, 22)
    assert Bitstring([1, 2, 4], 24) == Bitstring(66052, 24)
    assert Bitstring(66052, 22) != Bitstring(16513, 22)


def test_slice() -> None:
    b = Bitstring([1, 15, 240, 128], 32)
    b2 = Bitstring([2, 31, 225, 0], 31)
    assert b[7] == Bitstring(1, 1)
    assert b[6] == Bitstring(0, 1)
    assert b[1:] == b2
    assert b2[1:] == Bitstring([4, 63, 194, 0], 30)
    assert b[8:24] == Bitstring([15, 240], 16)
    assert b[12:20] == Bitstring([255], 8)
    assert b[:100] == b
    assert b[0:] == b
    assert b[0:2] == Bitstring([0], 2)


def test_repr() -> None:
    b = Bitstring([1, 2, 3], 20)
    assert repr(b) == "Bitstring([1, 2, 3], 20)"
