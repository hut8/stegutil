# stegutil
Bitmap Steganography in FORTRAN
stegutil provides methods for encoding arbitrary byte strings into uncompressed,
24-bit bitmap images, then later decoding this data, emphasizing resistance to
detection through pads and creating plausible deniability through "chaff" pads.

```

== Usage: stegutil <operation> <arguments>

== Operations:
 -=[ encode - encode the contents of <payload> into <carrier.bmp>,
     encoding the length and contents of <payload> with <pad> (if specified),
     into <package.bmp>, which is visually identical to <carrier.bmp>

     usage    : stegutil encode <payload> <carrier.bmp> [<padfile>] <package.bmp>
     required : <payload>     - an arbitrary file containing data to be encoded
                <carrier.bmp> - a 24-bit, uncompressed bitmap
                <package.bmp> - result of encoding data
     optional : <padfile>     - an arbitrary file, at least as large as
                            <carrier.bmp> - 54 bytes, known only to the sender
                and recipient (must be agreed upon in advance if
                used)

 -=[ decode - decode the contents of <package.bmp>, which is produced as a
     bitmap package encoded using this utility, encoding the length and
     extracted bits with <pad> (if specified), into <payload>

     usage    : stegutil decode <package.bmp> [<padfile>] <payload>
     required : <package.bmp> - 24-bit, uncompressed bitmap containing encoded
                                payload (produced with "encode" operation)
                <payload>     - result of decoding data
     optional : <padfile>     - an arbitrary file, at least as large as
                                <carrier.bmp> - 54 bytes, known only to the sender
                                and recipient (must be agreed upon in advance if
                                used)

 -=[ chaff - create <chaffpad> from <chaff> to be used with <package.bmp>.
     Should an adversary somehow correctly suspect <package.bmp> contains
     encoded data, but does not possess the correct pad, a chaff pad may be
     provided by the originator, which when combined with the package and
     decoded, will yield the contents of a (harmless) file <chaff>

     usage    : stegutil chaff <chaff> <package.bmp> <chaffpad>
     required : <chaff>       - arbitrary file which will result when decoding
                                of <package.bmp> is attempted using <chaffpad>
                                as pad
                <package.bmp> - 24-bit, uncompressed bitmap
                <chaffpad>    - result of operation

     Note: as a demonstration of plausible deniability, it is possible to create
     a <chaffpad> which, when "decoding" a <package.bmp> with no encoded data,
     padded with <chaffpad>, will still yield data from <chaff>

 -=[ Considerations for strong secrecy:
   -- Padding provides four functions:
    - Increases computational complexity of finding hidden data
      [an adversary can no longer simply attempt to decode each suspected bitmap
      with this utility -- they must now attempt to pad each bitmap with all
      possible data that the suspect's originator possesses, which immediately
      becomes nearly impossible because the pad could be derived from a passphrase
      committed to memory, or any data available on the Internet]
    - Concealing the length of the payload
      [without a pad, if the 4-byte integer derived from the LSB of the first 32
      colors in <package.bmp> is less than the filesize of the image minus the
      size of the bitmap header, it is very likely to contain encoded data of
      any format.  Without a pad, this detection method is otherwise very
      computationally easy and accurate]
    - Concealing any patterns in the payload
      [for instance, the majority of MSBs of an 8-bit ASCII payload will be 0,
      or certain file formats may contain a relatively predictible header or
      magic number, such as BMP, PDF, or JPEG]
    - Providing for the possibility of a "chaff" pad, or conversely,
      demonstrating that a malicious pad may be produced by the adversary or a
      third party, thus giving plausible deniability
    - A good pad will contain relatively uniformly random bytes.
      A bad pad will contain all null bytes :-)

  -- Keep <carrier.bmp> secret.  If an adversary obtains <carrier.bmp>, and sees
     that only the least significant bit in each color is different from5
     <payload.bmp>, it would strongly suggest that <payload.bmp> contains encoded
     data.

 +---------------------------------------------------------------------+
 | WARNING: No warranty provided with this software!  Use at own risk. |
 +---------------------------------------------------------------------+

```
