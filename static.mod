GFORTRAN module version '0' created from static.f90 on Wed Apr 28 17:24:27 2010
MD5:cc9cd470ec65e7340514ed0e2fe2b138 -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () () ()
() () ())

()

()

()

()

(2 'achar' '(intrinsic)' 'achar' 1 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN FUNCTION) (UNKNOWN 0 0 0 UNKNOWN ()) 0 0 ()
() 2 () () () 0 0)
3 'help_chaff' 'static' 'help_chaff' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (CHARACTER 1 0 0 CHARACTER ((
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '964'))) 0 0 () (CONSTANT (
CHARACTER 1 0 0 CHARACTER (())) 0 964
' -=[ chaff - create <chaffpad> from <chaff> to be used with <package.bmp>.\U0000000a     Should an adversary somehow correctly suspect <package.bmp> contains\U0000000a     encoded data, but does not possess the correct pad, a chaff pad may be\U0000000a     provided by the originator, which when combined with the package and\U0000000a     decoded, will yield the contents of a (harmless) file <chaff>\U0000000a\U0000000a     usage    : stegutil chaff <chaff> <package.bmp> <chaffpad>\U0000000a     required : <chaff>       - arbitrary file which will result when decoding\U0000000a                                of <package.bmp> is attempted using <chaffpad>\U0000000a                                as pad\U0000000a                <package.bmp> - 24-bit, uncompressed bitmap\U0000000a                <chaffpad>    - result of operation\U0000000a\U0000000a     Note: as a demonstration of plausible deniability, it is possible to create\U0000000a     a <chaffpad> which, when "decoding" a <package.bmp> with no encoded data,\U0000000a     padded with <chaffpad>, will still yield data from <chaff>')
() 0 () () () 0 0)
4 'help_considerations' 'static' 'help_considerations' 1 ((PARAMETER
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (CHARACTER 1 0 0
CHARACTER ((CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1735'))) 0 0 () (
CONSTANT (CHARACTER 1 0 0 CHARACTER (())) 0 1735
' -=[ Considerations for strong secrecy:\U0000000a   -- Padding provides four functions:\U0000000a    - Increases computational complexity of finding hidden data\U0000000a      [an adversary can no longer simply attempt to decode each suspected bitmap\U0000000a      with this utility -- they must now attempt to pad each bitmap with all\U0000000a      possible data that the suspect''s originator possesses, which immediately\U0000000a      becomes nearly impossible because the pad could be derived from a passphrase\U0000000a      committed to memory, or any data available on the Internet]\U0000000a    - Concealing the length of the payload\U0000000a      [without a pad, if the 4-byte integer derived from the LSB of the first 32\U0000000a      colors in <package.bmp> is less than the filesize of the image minus the\U0000000a      size of the bitmap header, it is very likely to contain encoded data of\U0000000a      any format.  Without a pad, this detection method is otherwise very\U0000000a      computationally easy and accurate]\U0000000a    - Concealing any patterns in the payload\U0000000a      [for instance, the majority of MSBs of an 8-bit ASCII payload will be 0,\U0000000a      or certain file formats may contain a relatively predictible header or\U0000000a      magic number, such as BMP, PDF, or JPEG]\U0000000a    - Providing for the possibility of a "chaff" pad, or conversely,\U0000000a      demonstrating that a malicious pad may be produced by the adversary or a\U0000000a      third party, thus giving plausible deniability\U0000000a    - A good pad will contain relatively uniformly random bytes.\U0000000a      A bad pad will contain all null bytes :-)\U0000000a\U0000000a  -- Keep <carrier.bmp> secret.  If an adversary obtains <carrier.bmp>, and sees\U0000000a     that only the least significant bit in each color is different from5\U0000000a     <payload.bmp>, it would strongly suggest that <payload.bmp> contains encoded\U0000000a     data.')
() 0 () () () 0 0)
5 'help_decode' 'static' 'help_decode' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (CHARACTER 1 0 0 CHARACTER ((
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '761'))) 0 0 () (CONSTANT (
CHARACTER 1 0 0 CHARACTER (())) 0 761
' -=[ decode - decode the contents of <package.bmp>, which is produced as a\U0000000a     bitmap package encoded using this utility, encoding the length and\U0000000a     extracted bits with <pad> (if specified), into <payload>\U0000000a\U0000000a     usage    : stegutil decode <package.bmp> [<padfile>] <payload>\U0000000a     required : <package.bmp> - 24-bit, uncompressed bitmap containing encoded\U0000000a                                payload (produced with "encode" operation)\U0000000a                <payload>     - result of decoding data\U0000000a     optional : <padfile>     - an arbitrary file, at least as large as\U0000000a                                <carrier.bmp> - 54 bytes, known only to the sender\U0000000a                                and recipient (must be agreed upon in advance if\U0000000a                                used)')
() 0 () () () 0 0)
6 'help_encode' 'static' 'help_encode' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (CHARACTER 1 0 0 CHARACTER ((
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '769'))) 0 0 () (CONSTANT (
CHARACTER 1 0 0 CHARACTER (())) 0 769
' -=[ encode - encode the contents of <payload> into <carrier.bmp>,\U0000000a     encoding the length and contents of <payload> with <pad> (if specified),\U0000000a     into <package.bmp>, which is visually identical to <carrier.bmp>\U0000000a\U0000000a     usage    : stegutil encode <payload> <carrier.bmp> [<padfile>] <package.bmp>\U0000000a     required : <payload>     - an arbitrary file containing data to be encoded\U0000000a                <carrier.bmp> - a 24-bit, uncompressed bitmap\U0000000a                <package.bmp> - result of encoding data\U0000000a     optional : <padfile>     - an arbitrary file, at least as large as\U0000000a                                <carrier.bmp> - 54 bytes, known only to the sender\U0000000a                                and recipient (must be agreed upon in advance if\U0000000a                                used)')
() 0 () () () 0 0)
7 'logo' 'static' 'logo' 1 ((PARAMETER UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE) (CHARACTER 1 0 0 CHARACTER ((CONSTANT (INTEGER 4
0 0 INTEGER ()) 0 '327'))) 0 0 () (CONSTANT (CHARACTER 1 0 0 CHARACTER (
())) 0 327
'  _____ _______ ______ _____ _    _ _______ _____ _\U0000000a / ____|__   __|  ____/ ____| |  | |__   __|_   _| |\U0000000a| (___    | |  | |__ | |  __| |  | |  | |    | | | |\U0000000a \\___ \\   | |  |  __|| | |_ | |  | |  | |    | | | |\U0000000a ____) |  | |  | |___| |__| | |__| |  | |   _| |_| |____\U0000000a|_____/   |_|  |______\\_____|\\____/   |_|  |_____|______|\U0000000a\U0000000a')
() 0 () () () 0 0)
8 'showhelp' 'static' 'showhelp' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN SUBROUTINE) (UNKNOWN 0 0 0 UNKNOWN ()) 9 0 (10)
() 0 () () () 0 0)
11 'static' 'static' 'static' 1 ((MODULE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN) (UNKNOWN 0 0 0 UNKNOWN ()) 0 0 () () 0 () () () 0 0)
12 'su_desc' 'static' 'su_desc' 1 ((PARAMETER UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE) (CHARACTER 1 0 0 CHARACTER ((
CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '239'))) 0 0 () (CONSTANT (
CHARACTER 1 0 0 CHARACTER (())) 0 239
'stegutil provides methods for encoding arbitrary byte strings into uncompressed,\U0000000a24-bit bitmap images, then later decoding this data, emphasizing resistance to\U0000000adetection through pads and creating plausible deniability through "chaff" pads.')
() 0 () () () 0 0)
10 'topic' '' 'topic' 9 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN DUMMY)
(CHARACTER 1 0 0 CHARACTER ((CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '255')))
0 0 () () 0 () () () 0 0)
)

('achar' 0 2 'help_chaff' 0 3 'help_considerations' 0 4 'help_decode' 0
5 'help_encode' 0 6 'logo' 0 7 'showhelp' 0 8 'static' 0 11 'su_desc' 0
12)
