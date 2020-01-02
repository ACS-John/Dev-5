library 'S:\Core\Library': fntop,fnxit, fnH2Init,fnH2AddText,fnH2AddComboA,fnHamster2
on error goto Ertn
gosub Enum
fntop(program$,'RPWork01 Hamster')
fnH2Init
fnH2AddText("eno"                   ,8                           )
fnH2AddText("dep"                   ,3                           )
fnH2AddText("Regular Hours  inp(01)",7,"PD",4.2,mask_pointtwo, 12)
fnH2AddText("OverTime Hours inp(02)",7,"PD",4.2,mask_pointtwo, 16)
fnH2AddText("Vacation Hours inp(03)",7,"PD",4.2,mask_pointtwo, 20)
fnH2AddText("Sick Hours     inp(04)",7,"PD",4.2,mask_pointtwo, 24)
fnH2AddText("Holiday Hours  inp(05)",7,"PD",4.2,mask_pointtwo, 28)
fnH2AddText("Other Compensa inp(06)",7,"PD",5.2,mask_pointtwo, 32)
fnH2AddText("inp(07)"               ,7,"PD",5.2,mask_pointtwo, 37)
fnH2AddText("inp(08)"               ,7,"PD",5.2,mask_pointtwo, 42)
fnH2AddText("inp(09)"               ,7,"PD",5.2,mask_pointtwo, 47)
fnH2AddText("inp(10)"               ,7,"PD",5.2,mask_pointtwo, 52)
fnH2AddText("inp(11)"               ,7,"PD",5.2,mask_pointtwo, 57)
fnH2AddText("inp(12)"               ,7,"PD",5.2,mask_pointtwo, 62)
fnH2AddText("inp(13)"               ,7,"PD",5.2,mask_pointtwo, 67)
fnH2AddText("inp(14)"               ,7,"PD",5.2,mask_pointtwo, 72)
fnH2AddText("inp(15)"               ,7,"PD",5.2,mask_pointtwo, 77)
fnH2AddText("inp(16)"               ,7,"PD",5.2,mask_pointtwo, 82)
fnH2AddText("inp(17)"               ,7,"PD",5.2,mask_pointtwo, 87)
fnH2AddText("inp(18)"               ,7,"PD",5.2,mask_pointtwo, 92)
fnH2AddText("inp(19)"               ,7,"PD",5.2,mask_pointtwo, 97)
fnH2AddText("inp(20)"               ,7,"PD",5.2,mask_pointtwo,102)
fnH2AddText("inp(21)"               ,7,"PD",5.2,mask_pointtwo,107)
fnH2AddText("inp(22)"               ,7,"PD",5.2,mask_pointtwo,112)
fnH2AddText("inp(23)"               ,7,"PD",5.2,mask_pointtwo,117)
fnH2AddText("inp(24)"               ,7,"PD",5.2,mask_pointtwo,122)
fnH2AddText("inp(25)"               ,7,"PD",5.2,mask_pointtwo,127)
fnH2AddText("inp(26)"               ,7,"PD",5.2,mask_pointtwo,132)
fnH2AddText("inp(27)"               ,7,"PD",5.2,mask_pointtwo,137)
fnH2AddText("inp(28)"               ,7,"PD",5.2,mask_pointtwo,142)
fnH2AddText("inp(29)"               ,7,"PD",5.2,mask_pointtwo,147)
fnH2AddText("GPD"                   ,7,"PD",5.2,mask_pointtwo,152)
fnH2AddText("hr(1)"                 ,7,"PD",4.2,mask_pointtwo,157)
fnH2AddText("hr(2)"                 ,7,"PD",4.2,mask_pointtwo,161)

fn_open_file : fn_close_file : fn_open_file
fnHamster2("RPWork")
fn_close_file
goto XIT

def fn_open_file
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: 'Name=[Q]\PRmstr\rpwork01.h[cno],Version=0,KFName=[Q]\PRmstr\rpwork01Idx.h[cno],Use,RecL=167,KPs=1,KLn=11,Shr',internal,outIn,keyed 
fnend 
def fn_close_file
	for j=1 to open_file_count : close #j: : next j
fnend  ! fn_close_file
XIT: fnxit
include: Enum
include: Ertn