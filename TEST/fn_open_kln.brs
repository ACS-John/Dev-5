00001   dim location$(0)*256,locationn(0)
00002   let hlfalocation=fn_open('U4 Meter Location',mat location$,mat locationn,mat form$, 1,4)
00003   print 'kln(hLfaLocation,1)=';kln(hlfalocation,1)
00004   end 
00005 ! openDataN_open (supressprompt:=2)
00006   dim form$(0)*2048
00008   def fn_open(opentablename$*255, mat opendata$, mat opendatan, mat openform$; openinputonly,openkeynumber,opendisableenumsort,openpath$*255,mat opendescription$,mat openfieldwidth, ___,openletenumitem,openreturn,opensupressprompt)
00009     library 'S:\Core\Library': fnopenfile
00010     dim openletenum$(0)*800
00011     dim openenumloaded$(0)*32
00012     let opensupressprompt=2
00013     let openreturn=fnopenfile(opentablename$,mat opendata$,mat opendatan,mat openform$, openinputonly,openkeynumber,opendisableenumsort,openpath$,mat opendescription$,mat openfieldwidth,mat openletenum$,opensupressprompt)
00014     let opentablename$=lwrc$(opentablename$)
00015     if srch(mat openenumloaded$,opentablename$)<=0 then 
00016       mat openenumloaded$(udim(mat openenumloaded$)+1)
00017       let openenumloaded$(udim(mat openenumloaded$))=opentablename$
00018       for openletenumitem=1 to udim(mat openletenum$)
00019         execute openletenum$(openletenumitem)
00020       next openletenumitem
00021     end if 
00022     let fn_open=openreturn
00023   fnend 
