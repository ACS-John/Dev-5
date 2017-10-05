00001   library 's:\core\library': fngethandle,fngetdir2,fnaddonec
00002   print newpage
00003 ! dim fileOld$*256
00004 ! dim fileNew$*256
00005   dim lineold$*512
00006   dim linenew$*512
00007   mat loclist$(0)
00008   fnaddonec(mat loclist$,'18')
00009   fnaddonec(mat loclist$,'19')
00010   fnaddonec(mat loclist$,'62')
00011   fnaddonec(mat loclist$,'63')
00012   fnaddonec(mat loclist$,'64')
00013   fnaddonec(mat loclist$,'65')
00014   fnaddonec(mat loclist$,'75')
00015   fnaddonec(mat loclist$,'77')
00016   fnaddonec(mat loclist$,'78')
00017   fnaddonec(mat loclist$,'79')
00018   fnaddonec(mat loclist$,'80')
00019   fnaddonec(mat loclist$,'81')
00020   fnaddonec(mat loclist$,'82')
00021   fnaddonec(mat loclist$,'LA')
00022   fnaddonec(mat loclist$,'OC')
00023   for locitem=1 to udim(mat loclist$)
00024     location$=loclist$(locitem)
00025     dim filenameold$(0)*256
00026     dim filenamenew$(0)*256
00027     open #hint:=fngethandle: 'name='&env$('temp')&'\diff.dat,kfname='&env$('temp')&'\diff.idx,recl=2048,kps=1,kln=8,replace',internal,outin,keyed 
00028     fngetdir2('C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\',mat filenameold$, '/s','e_q4.'&location$&'.*.*',mat unuseddate$,mat unusedtime$,1)
00029     fngetdir2('C:\Users\John\Desktop\pbjExport\' ,mat filenamenew$, '/s','e_q4.'&location$&'.*.*',mat unuseddate$,mat unusedtime$,1)
00030     for fileitem=1 to udim(filename$)
00031 ! .! fileOld$='C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\'&filename$(fileItem)
00032 ! .! fileNew$='C:\Users\John\Desktop\pbjExport\08-pbj_emp_q4_081916-082616\'&filename$(fileItem)
00033 ! .! fileOld$='C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\08-pbj_emp_q4_081916-082616\e_q4.64.08192016-08262016.xml'
00034 ! .! fileNew$='C:\Users\John\Desktop\pbjExport\08-pbj_emp_q4_081916-082616\e_q4.64.08192016-08262016.xml'
00035 ! 
00036       open #hold:=fngethandle: 'name='&filenameold$(fileitem),display,input 
00037       open #hnew:=fngethandle: 'name='&filenamenew$(fileitem),display,input 
00038       do 
00039         linput #hold: lineold$ eof DIFFFINIS
00040 READNEW: ! 
00041         linput #hnew: linenew$ eof DIFFFINIS
00042         if lineold$(1:len('<hireDate>'))='<hireDate>' then 
00043           hire$=lineold$(11:pos(lineold$,'<',13)-1)
00044         else if lineold$(1:len('<employeeId>'))='<employeeId>' then 
00045           empid$=lineold$(13:pos(lineold$,'<',13)-1)
00046           hire$=''
00047 ! .    ! pr 'Emp: '&empid$
00048         else if lineold$<>linenew$ then 
00049           term$=linenew$(18:27)
00050           if linenew$(1:len('<terminationDate>'))='<terminationDate>' and lineold$(1:len('<terminationDate>'))='<terminationDate>' then 
00051             write #hint,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,'*** Old: '&lineold$&'   New: '&linenew$ ! &'  FileNew:'&filenameNew$(fileItem)
00052             terminationdatechangecount+=1
00053           else if linenew$(1:len('<terminationDate>'))<>'<terminationDate>' then 
00054             write #hint,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,'*** Old: '&lineold$&'   New: '&linenew$ ! &'  FileNew:'&filenameNew$(fileItem)
00055             unusualcount+=1
00056             pause 
00057           else 
00058             write #hint,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,linenew$
00059             goto READNEW
00060           end if 
00061         end if 
00062       loop 
00063 DIFFFINIS: ! 
00064       close #hold: 
00065       close #hnew: 
00066     next fileitem
00067     if ~exists('C:\Users\John\Desktop\pbjFix') then execute 'mkdir C:\Users\John\Desktop\pbjFix'
00068     open #hdiff:=fngethandle: 'name=C:\Users\John\Desktop\pbjFix\Loc'&location$&'_Q4_Missed_Terms_Update.xml,recl=2048,replace',display,output 
00069     print #hdiff: '<?xml version="1.0" encoding="ASCII"?>'
00070     print #hdiff: '<nursingHomeData xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="nhpbj_2_00_0.xsd">'
00071     print #hdiff: '<header fileSpecVersion="2.00.0">'
00072     print #hdiff: '<facilityId>N046026</facilityId>'
00073     print #hdiff: '<stateCode>KS</stateCode>'
00074     print #hdiff: '<reportQuarter>4</reportQuarter>'
00075     print #hdiff: '<federalFiscalYear>2016</federalFiscalYear>'
00076     print #hdiff: '<softwareVendorName>RH Positive</softwareVendorName>'
00077     print #hdiff: '<softwareVendorEmail>ray.espinoza@adventistcare.org</softwareVendorEmail>'
00078     print #hdiff: '<softwareProductName>RH Positive</softwareProductName>'
00079     print #hdiff: '<softwareProductVersion>2016</softwareProductVersion>'
00080     print #hdiff: '</header>'
00081     print #hdiff: '<employees>'
00082     restore #hint: 
00083     do 
00084       read #hint,using 'form pos 1,v 18,v 10,v 10,v 374': emp$,hire$,term$,linenew$ eof REPORTFINIS
00085       if linenew$<>lineold$ then 
00086 ! .  ! print #hDiff: lineNew$
00087         print #hdiff: '<employee>'
00088         print #hdiff: '<employeeId>'&emp$&'</employeeId>'
00089         print #hdiff: '<hireDate>'&hire$&'</hireDate>'
00090         if trim$(term$)<>'' then 
00091           print #hdiff: '<terminationDate>'&term$&'</terminationDate>'
00092         end if 
00093         print #hdiff: '</employee>'
00094         lineold$=linenew$
00095       end if 
00096     loop 
00097 REPORTFINIS: ! 
00098     print #hdiff: '</employees>'
00099     print #hdiff: '</nursingHomeData>'
00100     close #hdiff: 
00101     close #hint: 
00102     if terminationdatechangecount>0 then print location$&' has '&str$(terminationdatechangecount)&' Termination Date Changes' : terminationdatechangecount=0
00103     if unusualcount>0 then print location$&' has '&str$(unusualcount)&' unusual diffs' : unusualcount=0
00104   next locitem
00105 ! 
