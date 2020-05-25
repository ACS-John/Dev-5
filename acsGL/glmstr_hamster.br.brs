 
autoLibrary
fnTop(program$)
gosub Enum
on error goto Ertn
fnH2Init
fnH2AddText("Account"           ,12                          )
fnH2AddText("Description"       ,50,"C" ,50,0            , 13)
fnH2AddText("Income Stmt Ref"   , 5,"PD", 3,mask_number  , 69)
fnH2AddText("Beginning Balance" ,11,"PD", 6,mask_pointtwo, 81)
fnH2AddText("Current Balance"   ,11,"PD", 6,mask_pointtwo, 87)
fnH2AddText("2-Yr Beginning"    ,11,"PD", 6,mask_pointtwo,327)
 
fn_open_file : fn_close_file : fn_open_file
fnHamster2("GLmstr")
fn_close_file
goto Xit
 
def fn_open_file
  open_file_count=0 ! this value is used in the close_file sub routine
  open #open_file_count+=1: "Name=[Q]\GLmstr\GLmstr.h[cno],Version=0,KFName=[Q]\GLmstr\glIndx2.h[cno],Use,RecL=416,KPs=13,KLn=30,Shr",internal,outIn,keyed
fnend
def fn_close_file
  for j=1 to open_file_count : close #j: : next j
fnend
Xit: fnXit
include: Enum
include: Ertn
