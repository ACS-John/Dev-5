library 's:\core\library': fngethandle,fngetdir2,fnAddOneC
pr newpage
! dim fileOld$*256
! dim fileNew$*256
dim lineOld$*512
dim lineNew$*512
mat locList$(0)
fnAddOneC(mat locList$,'18')
fnAddOneC(mat locList$,'19')
fnAddOneC(mat locList$,'62')
fnAddOneC(mat locList$,'63')
fnAddOneC(mat locList$,'64')
fnAddOneC(mat locList$,'65')
fnAddOneC(mat locList$,'75')
fnAddOneC(mat locList$,'77')
fnAddOneC(mat locList$,'78')
fnAddOneC(mat locList$,'79')
fnAddOneC(mat locList$,'80')
fnAddOneC(mat locList$,'81')
fnAddOneC(mat locList$,'82')
fnAddOneC(mat locList$,'LA')
fnAddOneC(mat locList$,'OC')
for locItem=1 to udim(mat locList$)
  location$=locList$(locItem)
  dim filenameOld$(0)*256
  dim filenameNew$(0)*256
  open #hInt:=fngethandle: 'name='&env$('temp')&'\diff.dat,kfname='&env$('temp')&'\diff.idx,recl=2048,kps=1,kln=8,replace',internal,outin,keyed
  fngetdir2('C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\',mat filenameOld$, '/s','e_q4.'&location$&'.*.*',mat unusedDate$,mat unusedTime$,1)
  fngetdir2('C:\Users\John\Desktop\pbjExport\'               ,mat filenameNew$, '/s','e_q4.'&location$&'.*.*',mat unusedDate$,mat unusedTime$,1)
  for fileItem=1 to udim(filename$)
    ! fileOld$='C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\'&filename$(fileItem)
    ! fileNew$='C:\Users\John\Desktop\pbjExport\08-pbj_emp_q4_081916-082616\'&filename$(fileItem)
    ! fileOld$='C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\08-pbj_emp_q4_081916-082616\e_q4.64.08192016-08262016.xml'
    ! fileNew$='C:\Users\John\Desktop\pbjExport\08-pbj_emp_q4_081916-082616\e_q4.64.08192016-08262016.xml'
    
    open #hOld:=fngethandle: 'name='&filenameOld$(fileItem),d,i
    open #hNew:=fngethandle: 'name='&filenameNew$(fileItem),d,i
    do
      linput #hOld: lineOld$ eof DiffFinis
      ReadNew: !
      linput #hNew: lineNew$ eof DiffFinis
      if lineOld$(1:len('<hireDate>'))='<hireDate>' then
        hire$=lineOld$(11:pos(lineOld$,'<',13)-1)
      else if lineOld$(1:len('<employeeId>'))='<employeeId>' then
        empid$=lineOld$(13:pos(lineOld$,'<',13)-1)
        hire$=''
        ! pr 'Emp: '&empid$
      else if lineOld$<>lineNew$ then
        term$=lineNew$(18:27)
        if lineNew$(1:len('<terminationDate>'))='<terminationDate>' and lineOld$(1:len('<terminationDate>'))='<terminationDate>' then
          write #hInt,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,'*** Old: '&lineOld$&'   New: '&lineNew$ ! &'  FileNew:'&filenameNew$(fileItem)
          TerminationDateChangeCount+=1
        else if lineNew$(1:len('<terminationDate>'))<>'<terminationDate>' then 
          write #hInt,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,'*** Old: '&lineOld$&'   New: '&lineNew$ ! &'  FileNew:'&filenameNew$(fileItem)
          unusualCount+=1
          pause
        else
          write #hInt,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,lineNew$
          goto ReadNew
        end if
      end if
    loop
    DiffFinis: !
    close #hOld:
    close #hNew:
  next fileItem
  if ~exists('C:\Users\John\Desktop\pbjFix') then exe 'mkdir C:\Users\John\Desktop\pbjFix'
  open #hDiff:=fngethandle: 'name=C:\Users\John\Desktop\pbjFix\Loc'&location$&'_Q4_Missed_Terms_Update.xml,recl=2048,replace',d,o
  pr #hDiff: '<?xml version="1.0" encoding="ASCII"?>'
  pr #hDiff: '<nursingHomeData xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="nhpbj_2_00_0.xsd">'
  pr #hDiff: '<header fileSpecVersion="2.00.0">'
  pr #hDiff: '<facilityId>N046026</facilityId>'
  pr #hDiff: '<stateCode>KS</stateCode>'
  pr #hDiff: '<reportQuarter>4</reportQuarter>'
  pr #hDiff: '<federalFiscalYear>2016</federalFiscalYear>'
  pr #hDiff: '<softwareVendorName>RH Positive</softwareVendorName>'
  pr #hDiff: '<softwareVendorEmail>ray.espinoza@adventistcare.org</softwareVendorEmail>'
  pr #hDiff: '<softwareProductName>RH Positive</softwareProductName>'
  pr #hDiff: '<softwareProductVersion>2016</softwareProductVersion>'
  pr #hDiff: '</header>'
  pr #hDiff: '<employees>'
  restore #hInt:
  do
    read #hInt,using 'form pos 1,v 18,v 10,v 10,v 374': emp$,hire$,term$,lineNew$ eof ReportFinis
    if lineNew$<>lineOld$ then
      ! print #hDiff: lineNew$
      print #hDiff:'<employee>'
      print #hDiff: '<employeeId>'&emp$&'</employeeId>'
      print #hDiff: '<hireDate>'&hire$&'</hireDate>'
      if trim$(term$)<>'' then
        print #hDiff: '<terminationDate>'&term$&'</terminationDate>'
      end if
      print #hDiff:'</employee>'
      lineOld$=lineNew$
    end if
  loop
  ReportFinis: !
  pr #hDiff: '</employees>'
  pr #hDiff: '</nursingHomeData>'
  close #hDiff:
  close #hInt:
  if TerminationDateChangeCount>0 then pr location$&' has '&str$(TerminationDateChangeCount)&' Termination Date Changes' : TerminationDateChangeCount=0
  if unusualCount>0 then pr location$&' has '&str$(unusualCount)&' unusual diffs' : unusualCount=0
next locItem

