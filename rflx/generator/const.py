from rflx import ada
from rflx.common import file_name

REFINEMENT_PACKAGE = ada.ID("Contains")

ARITHMETIC_PACKAGE = ada.ID("RFLX_Arithmetic")
BUILTIN_TYPES_CONVERSIONS_PACKAGE = ada.ID("RFLX_Builtin_Types.Conversions")
BUILTIN_TYPES_PACKAGE = ada.ID("RFLX_Builtin_Types")
GENERIC_TYPES_PACKAGE = ada.ID("RFLX_Generic_Types")
MESSAGE_SEQUENCE_PACKAGE = ada.ID("RFLX_Message_Sequence")
SCALAR_SEQUENCE_PACKAGE = ada.ID("RFLX_Scalar_Sequence")
TYPES_PACKAGE = ada.ID("RFLX_Types")

LIBRARY_FILES = [
    file_name(str(p)) + ".ads"
    for p in [
        ARITHMETIC_PACKAGE,
        BUILTIN_TYPES_CONVERSIONS_PACKAGE,
        BUILTIN_TYPES_PACKAGE,
        GENERIC_TYPES_PACKAGE,
        MESSAGE_SEQUENCE_PACKAGE,
        SCALAR_SEQUENCE_PACKAGE,
        TYPES_PACKAGE,
    ]
] + [
    file_name(str(p)) + ".adb"
    for p in [
        ARITHMETIC_PACKAGE,
        GENERIC_TYPES_PACKAGE,
        MESSAGE_SEQUENCE_PACKAGE,
        SCALAR_SEQUENCE_PACKAGE,
    ]
]

TEMPLATE_DIR = ("rflx", "templates/")

TYPES = ada.ID("Types")
TYPES_BYTE = TYPES * "Byte"
TYPES_BYTES = TYPES * "Bytes"
TYPES_BYTES_PTR = TYPES * "Bytes_Ptr"
TYPES_INDEX = TYPES * "Index"
TYPES_LENGTH = TYPES * "Length"
TYPES_BIT_INDEX = TYPES * "Bit_Index"
TYPES_BIT_LENGTH = TYPES * "Bit_Length"
TYPES_BYTE_INDEX = TYPES * "Byte_Index"
TYPES_FIRST_BIT_INDEX = TYPES * "First_Bit_Index"
TYPES_LAST_BIT_INDEX = TYPES * "Last_Bit_Index"
TYPES_OFFSET = TYPES * "Offset"
TYPES_UNREACHABLE_BIT_LENGTH = TYPES * "Unreachable_Bit_Length"
TYPES_U64 = TYPES * "U64"

CONTEXT_INVARIANT = [
    ada.Equal(e, ada.Old(e))
    for e in (
        ada.Variable("Ctx.Buffer_First"),
        ada.Variable("Ctx.Buffer_Last"),
        ada.Variable("Ctx.First"),
        ada.Variable("Ctx.Last"),
    )
]
