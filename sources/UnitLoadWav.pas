unit UnitLoadWav;
 
interface
type
  TCCArray=array of array of cardinal;
  TBBArray=array of array of byte;
  TWWArray=array of array of Word;

type TDDArray=array of array of double;
function LoadWaveFile(WaveFileName:String;var Y:TDDArray;var NumChannels,BitsPerSample,SampleRate,NumSamples:Cardinal):Boolean;
 
implementation
uses classes,sysutils;
 
type
//Les différents headers que l'on trouve dans un fichier WAV
  THeaderRIFF = Packed Record
    ID    : Array[1..4]Of Char;
    Taille: Cardinal;
    Format: Array[1..4]Of Char;
  end;
 
  THeaderFormat = Packed Record
    ID    : Array[1..4]Of Char;
    Taille: Cardinal;
    Format: WORD;
    Mode  : WORD;
    SRate : Cardinal; //=Fréquence d'échantillonnage
    BRate : Cardinal; //=Nombre d'octets pour une seconde (SRate * Mode * BPS/8)
    Align : WORD;
    BPS   : WORD;     //Bits Per Sample
  end;
 
  THeaderData = Packed Record
    ID    : Array[1..4]Of Char;
    Taille: Cardinal;
  end;
 
  TWAVE = Packed Record
    RIFF: THeaderRIFF;
    Fmt : THeaderFormat;
    Data: THeaderData;
  end;
 
 
// Reads file WaveFileName. Puts the normalized wave data in array Y and returns the sampling rate in SampleRate.
// On return Y will hold a multidimensional array Y (sample,channel) of type Double, with the audio data.
// Returns True, if the operation was successful, else False.
 
function LoadWaveFile(WaveFileName:String;var Y:TDDArray;var NumChannels,BitsPerSample,SampleRate,NumSamples:Cardinal):Boolean;
var
  WAVE:TWAVE;
  Stream: TFileStream;
  tmp   : String;
  i,c,nn:Cardinal;
  ybyte:TBBArray;
  yword:TWWArray;
begin
  Result:=false;
  Stream := TFileStream.Create(WaveFileName, fmOpenRead);
 
  Try
    //Lecture des trois headers
    Stream.Read(WAVE, 44);
 
    //Vérification de la validité du fichier. Il faut un WAVE PCM
    If (WAVE.RIFF.ID <> 'RIFF') or (WAVE.RIFF.Format <> 'WAVE') or
       (WAVE.Fmt .ID <> 'fmt ') or (WAVE.Fmt.Format  <> 1) then
      Raise exception.Create('Le fichier choisi n''est pas un fichier WAVE PCM valide');
 
    Result:=True;
 
    NumSamples:=Round(WAVE.Data.Taille/(WAVE.Fmt.BPS/8)/WAVE.Fmt.Mode);
    NumChannels:=WAVE.Fmt.Mode;
    SampleRate:=WAVE.Fmt.SRate;
    BitsPerSample:=WAVE.Fmt.BPS;
 
    SetLength(Y,NumSamples,NumChannels);
    //FillChar(Y,SizeOf(y),0);
 
    if BitsPerSample=8
    then begin
           SetLength(ybyte,NumChannels,NumSamples);
           nn:=NumChannels*NumSamples*sizeof(byte);
           stream.Read(ybyte[0,0],nn);
           for i:= 0 to NumSamples - 1 do
             for c:= 0 to NumChannels - 1 do
               Y[i,c]:=(ybyte[c,i]-128)/128  //8 Bits/sample est non signé
         end
    else if BitsPerSample=16
         then begin
                SetLength(yword,NumChannels,NumSamples);
                nn:=NumChannels*NumSamples*sizeof(Word);
                stream.Read(yword[0,0],nn);
                for i:= 0 to NumSamples - 1 do
                  for c:= 0 to NumChannels - 1 do
                    Y[i,c]:=(yword[c,i]/32768);
              end
         else ShowMessage(IntToStr(BitsPerSample)+' bits/sample non supporté');
  finally
    Stream.Free;
  end;
end;
end.
