autoLibrary
on error goto Ertn
gosub Enum
fnTop(program$)
! r: layout
fnH2Init
fnH2AddText('eno'                   ,8                            )
fnH2AddText('dep'                   ,3                            )
fnH2AddText('Regular Hours  inp(1)',7,'PD',4.2,mask_pointtwo, 12)
fnH2AddText('OverTime Hours inp(2)',7,'PD',4.2,mask_pointtwo, 16)
fnH2AddText('Vacation Hours inp(3)',7,'PD',4.2,mask_pointtwo, 20)
fnH2AddText('Sick Hours     inp(4)',7,'PD',4.2,mask_pointtwo, 24)
fnH2AddText('Holiday Hours  inp(5)',7,'PD',4.2,mask_pointtwo, 28)
fnH2AddText('Other Compensa inp(6)',7,'PD',5.2,mask_pointtwo, 32)
fnH2AddText('inp(07)'               ,7,'PD',5.2,mask_pointtwo, 37)
fnH2AddText('inp(08)'               ,7,'PD',5.2,mask_pointtwo, 42)
fnH2AddText('inp(09)'               ,7,'PD',5.2,mask_pointtwo, 47)
fnH2AddText('inp(10)'               ,7,'PD',5.2,mask_pointtwo, 52)
fnH2AddText('inp(11)'               ,7,'PD',5.2,mask_pointtwo, 57)
fnH2AddText('inp(12)'               ,7,'PD',5.2,mask_pointtwo, 62)
fnH2AddText('inp(13)'               ,7,'PD',5.2,mask_pointtwo, 67)
fnH2AddText('inp(14)'               ,7,'PD',5.2,mask_pointtwo, 72)
fnH2AddText('inp(15)'               ,7,'PD',5.2,mask_pointtwo, 77)
fnH2AddText('inp(16)'               ,7,'PD',5.2,mask_pointtwo, 82)
fnH2AddText('inp(17)'               ,7,'PD',5.2,mask_pointtwo, 87)
fnH2AddText('inp(18)'               ,7,'PD',5.2,mask_pointtwo, 92)
fnH2AddText('inp(19)'               ,7,'PD',5.2,mask_pointtwo, 97)
fnH2AddText('inp(20)'               ,7,'PD',5.2,mask_pointtwo,102)
fnH2AddText('inp(21)'               ,7,'PD',5.2,mask_pointtwo,107)
fnH2AddText('inp(22)'               ,7,'PD',5.2,mask_pointtwo,112)
fnH2AddText('inp(23)'               ,7,'PD',5.2,mask_pointtwo,117)
fnH2AddText('inp(24)'               ,7,'PD',5.2,mask_pointtwo,122)
fnH2AddText('inp(25)'               ,7,'PD',5.2,mask_pointtwo,127)
fnH2AddText('inp(26)'               ,7,'PD',5.2,mask_pointtwo,132)
fnH2AddText('inp(27)'               ,7,'PD',5.2,mask_pointtwo,137)
fnH2AddText('inp(28)'               ,7,'PD',5.2,mask_pointtwo,142)
fnH2AddText('inp(29)'               ,7,'PD',5.2,mask_pointtwo,147)
fnH2AddText('GPD'                   ,7,'PD',5.2,mask_pointtwo,152)
fnH2AddText('hr(1)'                 ,7,'PD',4.2,mask_pointtwo,157)
fnH2AddText('hr(2)'                 ,7,'PD',4.2,mask_pointtwo,161)
fnH2AddText('PLAWA'                 ,7,'PD',4.2,mask_pointtwo,165)
! /r
open #1: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],Version=0,KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno],Use,RecL=168,KPs=1,KLn=11,Shr',i,outIn,k
fnHamster2('Timesheet')
close #1:
goto Xit
 
Xit: fnXit
include: Enum
include: ertn
