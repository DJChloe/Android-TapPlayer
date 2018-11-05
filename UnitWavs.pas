unit UnitWavs;

interface

type TheadArray=ARRAY[0..43]OF BYTE;
 CONST HEADER:TheadArray=(
    $52,$49,$46,$46, $00,$00,$00,$00, $57,$41,$56,$45, $66,$6D,$74,$20,
    $10,$00,$00,$00, $01,$00,$01,$00, $44,$AC,$00,$00, $88,$58,$01,$00,
    $02,$00,$10,$00, $64,$61,$74,$61, $00,$00,$00,$00);
type
  THEAD=packed RECORD
        case b:boolean of
        True:(AsBytes:TheadArray);
        false:(Tag1       : ARRAY[1..4]OF CHAR;  { 00..03  Constante "RIFF"       }
              Size1      : LongWord;             { 04..07  Filesize-8             }
              Tag2       : ARRAY[1..14]OF CHAR; { 08..21  Constante "WAVEfmt..." }
              Mode       : WORD;                { 22..23  Mono or Stereo         }
              Freq       : LongWord;             { 24..27  Frequence (Hz)         }
              BytePerSec : LongWord;             { 28..31  Freq*NbrByte           }
              NbrByte    : WORD;                { 32..33  (Format div 8)*Mode    }
              Format     : WORD;                { 34..35  8 or 16 bits           }
              Tag3       : ARRAY[1..4]OF CHAR;  { 36..39  Constante "data"       }
              Size2      : LongWord);             { 40..43  Filesize-44           }
        end;

implementation

end.
