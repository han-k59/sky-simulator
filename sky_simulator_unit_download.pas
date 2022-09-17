unit sky_simulator_unit_download;

{$mode delphi}

interface
uses
  sysutils,
  classes,
  fphttpclient,
  openssl,
  opensslsockets;

function download_file(url, filename:string):boolean;{download file}

implementation


function download_file(url, filename:string):boolean;{download file}
var
  Client: TFPHttpClient;
  FS: TStream;
  SL: TStringList;

begin
  { SSL initialization has to be done by hand here }
  InitSSLInterface;

  result:=true;
  Client := TFPHttpClient.Create(nil);
  FS := TFileStream.Create(Filename,fmCreate or fmOpenWrite);
  try
    try
      { Allow redirections }
      Client.AllowRedirect := true;
      Client.Get(url,FS);
    except
        result:=false;
     // on E: EHttpClient do
     //   writeln(E.Message)
     // else
     //   raise;
    end;
  finally
    FS.Free;
    Client.Free;
  end;
end;

end.

