program test;

{ Program to demonstrate the Select function. }

uses 
  BaseUnix;

var 
  FDS: Tfdset;
  msg: String;

begin
  while true do
  begin
    fpfd_zero(FDS);
    fpfd_set(0,FDS);
    Writeln ('Enter some text...');
    { Wait until File descriptor 0 (=Input) changes }
    fpSelect (1,@FDS,nil,nil,nil);
    { Get rid of <ENTER> in buffer }
    readln(msg);
    Writeln ('You said: '+msg);
  end;
//  Fpfd_zero(FDS);
//  FpFd_set (0,FDS);
//  if fpSelect (1,@FDS,nil,nil,2000)>0 then
//    Writeln ('Thank you !')
//    { FD_ISSET(0,FDS) would be true here. }
//  else
//    Writeln ('Too late !');
end.