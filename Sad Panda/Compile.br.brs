10000 ! exe 'lo "'&program$(1:pos(program$,'.')-1)&'.br"'
10300 ! execute 'config editor "c:\program files\notepad++\notepad++.exe"'
10600 ! ed ~                     
10900 ! ** do not remove line numbers nor use syntax that requires a pre-compilier in this program - or you're not gonna have a good time **
10920 ! ** renumber all you like
11200 dim pandaPath$*256
11500 pandaPath$=program$(1:pos(program$,'\',-1)-1) ! pandaPath$=env$('cmd_home')(without a trailing backslash
11800 fn_conSub('[pandaPath]',pandaPath$)
12100 setenv('pandaPath',pandaPath$)
12400 if env$('returningFromPlugInProc')='' t exe 'loa "'&env$('pandaPath')&'\BambooForest.br",Resident'
12700 library env$('pandaPath')&'\BambooForest.br': fnGetHandle,fnGetPp,fnBackgroundPicture$,fnReadFileIntoArray
13000 library env$('pandaPath')&'\BambooForest.br': fnStatus,fnStatusPause
13300 library env$('pandaPath')&'\BambooForest.br': fnHasInitialScan,fnHasKeyAdd
13600 ! library '[pandaPath]\BambooForest.br': <--- fyi, that doesn't work.
13900 ! r: basic tests on file specified (in env$('name')) to confirm the program will work properly
14200 dim orgSourceFile$*256            ! full name (without quotes) of the program source file we are compiling.
14500 orgSourceFile$=trim$(env$('name'),'"')
14800 multiFile=0
15100 if orgSourceFile$='' t
15400   pr 'This program requires env$("name") be set to the source code to compile.'
15700   pr 'Your env$("name") is blank.  Program stopping.'
16000   en
16300 else if orgSourceFile$(1:len(program$))=program$ t
16600   pr 'If you try to use this program to compile itself, you''re probably not gonna have a good time.'
16900   pr 'Program stopping.'
17200   en
17500 else if 'filelist:'=orgSourceFile$(1:len('filelist:')) t
17800   ! pr 'filelist: detected in orgSourceFile$ ('&orgSourceFile$&')'
18100   multiFile=1
18400 en if
18700 ! /r
19000 on err got ErrorHandler
19300 setenv('icon',env$('pandaPath')&'\asset\sadPanda32.ico')
19600
19900 if multiFile then
20200     dim orgFiles$(0)*512
20500     fileCount=fnReadFileIntoArray(orgSourceFile$(10:inf),mat orgFiles$)
20800 else
21100   fileCount=1
21400   mat orgFiles$(1)
21700   orgFiles$(1)=orgSourceFile$
22000 end if
22300 for i=1 to fileCount
22320   ! pr 'TOP of great i loop.  i=';i : pause
22330   ! 
22600   if ~exists(orgFiles$(i)) t
22900     pr 'It appears the file you are trying to compile does not exist.'
23200     pr 'Program stopping.'
23500     en
23800   en if
24400   ! r: Defaults to be overriden by subproc Developer Settings.proc
24700   setenv('background_picture','[pandaPath]\wallpaper\52H.jpg') ! 52H or 39H ! [pandaPath] only works in env$('background_picture') and it just gets replaced with pandaPath$ after pandaPath$ is set.  (smoke and mirrors)  This is done so that this developer prefernce region can be set at the top of the logic flow
25000   setEnv('path-include','C:\ACS\Dev-5\^resource\')
25300   setEnv('ext-include','.brs')
25600   setenv('keyword-include','include:')
25900   setenv('exe-examDiff','C:\Program Files\ExamDiff Pro\ExamDiff.exe')
26200   ! by default we're in a vareity of different places depending upon how the Compile.cmd was called.  Temp is, at least, writable.   This isn't necessary, all paths are fully qualified but I leave some procs there that make my life easier. -John
26500   if env$('temp')(2:2)=':' t
26800     exe 'CD '&env$('temp')(1:2)
27100     exe 'CD '&env$('temp')(3:len(env$('temp')))
27400   en if
27700   ! /r
28000

30700
31000   ! r: read Developer Settings
31600   dim simpleSrepFrom$(0)*256
31900   dim simpleSrepTo$(0)*256
32500   dim developerSettingsFilename$*256
32800   developerSettingsFilename$=pandaPath$&'\Developer Settings.proc'
33100   dim developerSettingsProcLine$(0)*800
33400   if exists(developerSettingsFilename$) t
33700     exe 'subproc '&developerSettingsFilename$
34000     fnReadFileIntoArray(developerSettingsFilename$,mat developerSettingsProcLine$)
34300   en if
34600   ! /r

35000   if env$('returningFromPlugInProc')='True' t
35020     setenv('returningFromPlugInProc','')
35040     i=val(env$('i'))+1
35050     if i>fileCount then goto PostI
35060     ! pr 'I see you are "returningFromPlugInProc" setting i=';i : pause
35080   en if 
35100   dim tmpSourceFile$*256
35120   dim outCompileFile$*256
35140   fn_prepareLocalVariables(orgFiles$(i),tmpSourceFile$,outCompileFile$)
35220   ! pr 'after fn_prepareLocalVariables' : pause
35240   setenv('pandaSourceFile',tmpSourceFile$)
35260   if env$('returningFromPlugInProc')='True' t
35280     got AfterRunPlugIns
35300   en if

36700   ! r: setup screen and display some initial collected data in fnstatus
37000   exe 'con gui on'
37300   fn_apply_theme
37600   ope #0: 'SRow=1,SCol=2,Rows=35,Cols=115,Picture='&fnBackgroundPicture$&',border=S:[screen],N=[screen],Caption=Sad Panda Compiler',display,outIn
37900   
38200   ope #winData:=fnGetHandle: 'SRow=32,SCol=15,Rows=3,Cols=80,border=S:[screen],N=[screen],Caption=Data,NoClose',display,outIn
38500   lc=0 
38800   pr #winData,f str$(lc+=1)&',1,Cr 15,[label]': 'File In:'   : pr #winData,f str$(lc)&',17,50/C 256,[textBox]': orgFiles$(i)
39100   pr #winData,f str$(lc+=1)&',1,Cr 15,[label]': 'File temp:' : pr #winData,f str$(lc)&',17,50/C 256,[textBox]': tmpSourceFile$
39400   pr #winData,f str$(lc+=1)&',1,Cr 15,[label]': 'Compile:'   : pr #winData,f str$(lc)&',17,50/C 256,[textBox]': outCompileFile$
39700   fnStatus('          Origional Source:    '&orgFiles$(i)   )
40000   fnStatus('   Post Pre-Compile Source:    '&tmpSourceFile$ )
40300   fnStatus('             Final Compile:    '&outCompileFile$)
40600   fnStatus('developer settings processed')
40900   for dsItem=1 to udim(mat developerSettingsProcLine$)
41200     if trim$(developerSettingsProcLine$(dsItem))(1:1)<>'!' and trim$(developerSettingsProcLine$(dsItem))<>'' t
41500       fnStatus(developerSettingsProcLine$(dsItem))
41800     en if
42100   n dsItem
42400   ! fnStatusPause ! pau ! /r
42700   ! r: *** main process ***
43000   fnStatus('Starting Pre-Compile')
43300   exec 'copy "'&orgFiles$(i)&'" "'&tmpSourceFile$&'"'
43600   ! r: whole file processes
43900   dim exclamationColon$*2
44200   exclamationColon$=chr$(ord('!'))&':' ! chr$(ord('!')) evaluates to an exclamation mark, but prevents a compile issue with the exclamation mark colon not doing what it normally does.
44500   fnHasKeyAdd(exclamationColon$        )
44800   !
45100   fnHasKeyAdd(chr$(9)           ) 
45400   for simpleSrepItem=1 to udim(mat simpleSrepFrom$)
45700    fnHasKeyAdd(simpleSrepFrom$(simpleSrepItem)) 
46000   n simpleSrepItem
46300   ! fnHasKeyAdd('[ifWithoutThen]'               ) !  add ' then' to the end of lines that have an 'if' but no 'then'
46600   !
46900   fnHasInitialScan(tmpSourceFile$) ! todo: add collections for varialbes and functions, variables-di/dim/init-ed-or-not, add collection for labels and a count of each label's references
47200   ! pau !
47500   if enablePlugIns t ! le fn_runPlugins(pandaPath$)
47800
48100     ! def fn_runPlugins(pandaPath$*256; ___,dir$*256,hDir,line$*800,plugInProcName$*256,plugInProcCount,plugInProcNameRepeat$*256)
48400     dim dir$*256,hDir,line$*800,plugInProcName$*256,plugInProcCount,plugInProcNameRepeat$*256
48700     dir$=line$=plugInProcName$=plugInProcNameRepeat$='' : hDir=plugInProcCount=0
49000     setenv('filename',tmpSourceFile$)
49300     exe 'sy dir /on /ad /b "'&pandaPath$&'\PlugIns\*." >"'&env$('temp')&'\SadPandaDir'&session$&'.txt"'
49600     ope #hDir:=fnGetHandle: 'name='&env$('temp')&'\SadPandaDir'&session$&'.txt',d,i
49900     plugInProcName$=env$('temp')&'\SadPanda_plugInProc'&session$&'.proc'
50200     ope #hPp:=fnGetHandle: 'name='&plugInProcName$&',recl=2048,replace',d,o
50500     do
50800       lin #hDir: line$ eof EoDirFile
51100       if exists(pandaPath$&'\PlugIns\'&line$&'\start.br') t
51400         plugInProcCount+=1
51700         fnStatus('detected PlugIn: '&line$)
52000         pr #hPp: 'loa "'&pandaPath$&'\PlugIns\'&line$&'\start.br"'
52300         plugInProcNameRepeat$=''
52600         pr #hPp: 'ru'
52900       en if
53200     loop
53500     EoDirFile: !
53800
54100   pr #hPp: 'Load "'&tmpSourceFile$&'",Source'
54400   pr #hPp:'Pr B: "*'&orgFiles$(i)&' - Sad Panda"'
54700   if exists(outCompileFile$) t
55000     pr #hPp: 'Replace "'&outCompileFile$&'"'
55300   else
55600     pr #hPp: 'Save "'&outCompileFile$&'"'
55900   en if
56200   pr #hPp: 'Free "'&tmpSourceFile$&'"'
56500   pr #hPp: ''
56800
57100
57400     if plugInProcCount>0 t
57700       pr #hPp: 'loa "'&program$&'"'
58000       setenv('returningFromPlugInProc','True')
58300       setenv('i',str$(i))
58600       pr #hPp: 'ru'
58900       cl #hPp:
59200       ! pr 'aaa' : pause
59500       cl #hDir,free:
59800       exe 'subproc '&plugInProcName$&''
60100       pr 'bbb' : pause ! this never happens, because it doesn't come back to here after the subproc
60400     en if
60700     cl #hDir,free:
61000     ! fn
61300   en if
61600
61900
62200
62500   AfterRunPlugIns: !
62800   ! pr 'Welcome to AfterRunPlugIns' : pause
63100   fnStatus('plugins completed.')
63400   ! fn_applySimpleSrep(tmpSourceFile$,mat simpleSrepFrom$,mat simpleSrepTo$)
63700   ! ! /r
64000   exe '*free "'&env$('runProc')&'" -n' ioerr ignore
64300   ! /r
64600
64900 n i
65200 PostI: !
65310 fnStatus('Completed compiling '&str$(fileCount)&' programs.')
65320 ! fnStatusPause
65400 pr 'program complete.' : execute 'sy'
65500 ! ! r: make and execute a subproc to do the actual compile and exit BR!
65800 ! dim tmpProcFile$*256
66100 ! tmpProcFile$=env$('temp')&'\SadPandaSession'&session$&'.proc' ! '.$$$'
66400 ! ope #hEd:=fnGetHandle: 'name='&tmpProcFile$&',recl=2048,use',d,o
66700 ! for ix=1 to fileCount
67000 !   fn_prepareLocalVariables(orgFiles$(ix),tmpSourceFile$,outCompileFile$)
67300 !   pr #hEd: 'Load "'&tmpSourceFile$&'",Source'
67600 !   pr #hEd:'Pr B: "*'&orgFiles$(ix)&' - Sad Panda"'
67900 !   if exists(outCompileFile$) t
68200 !     pr #hEd: 'Replace "'&outCompileFile$&'"'
68500 !   else
68800 !     pr #hEd: 'Save "'&outCompileFile$&'"'
69100 !   en if
69400 !   pr #hEd: 'Free "'&tmpSourceFile$&'"'
69700 !   pr #hEd: ''
70000 ! n ix
70300 ! cl #hEd:
70600 ! exe 'subproc '&tmpProcFile$&''
70900 ! /r

71800 def fn_prepareLocalVariables(orgSourceFile$*512,&tmpSourceFile$,&outCompileFile$; ___,folder$*256,filenameNoPathNoExt$*256,filenameExt$*64)
72100   fnGetPp(orgSourceFile$,folder$,filenameNoPathNoExt$,filenameExt$)
72400   tmpSourceFile$=env$('temp')&'\Sad Panda '&filenameNoPathNoExt$&' session'&session$&'.brs'
72700   posLastSlash=pos(orgSourceFile$,'\',-1)
73000   outCompileFile$=orgSourceFile$(1:pos(orgSourceFile$,'.',posLastSlash)-1) ! this looks for the first . in a program name and strips everything after that off to get the name without the extensions .br.brs .brs .wb.wbs and .wbs this method may be overkill if there are program names with any other periods in them
73300   if pos(orgSourceFile$,'.wbs')>0 t
73600     outCompileFile$(inf:inf)='.wb'
73900   else
74200     outCompileFile$(inf:inf)='.br'
74500   en if
74800 fn
75100
75400 def fn_apply_theme(; disableConScreenOpenDflt) ! from C:\ACS\Dev-5\Core\Programs\Preferences.br.brs
75700   min_fontsize_height$='14'                                      ! fnureg_read('Min_FontSize_Height',min_fontsize_height$)
76000   min_fontsize_width$='6'                                        ! fnureg_read('Min_FontSize_Width',min_fontsize_width$)
76300   exe 'config Min_FontSize '&min_fontsize_height$&'x'&min_fontsize_width$
76600   fn_set_color('[screen]','#000000','#E7EDF5')
76900 ! fn_set_color('[screen]','#000000','#E7EDF5')
77200   fn_set_color('[screenHeader]','#000000','#FFFFFF')
77500   fn_set_color('[textBox]','#000000','#FFFFFF')
77800   fn_set_color('[gridHeader]','#000000','#FFFFFF')
78100   fn_set_color('[label]','#000000','#B0C4DE')
78400   fn_set_color('[button]','#000000','#74DF00') ! '#F0F8FF')
78700   fn_set_color('[buttonCancel]','#000000','#CD5C5C')
79000   if ~disableConScreenOpenDflt t
79300     exe 'Config Screen OpenDflt "Rows=35, Cols=115, Picture='&fnBackgroundPicture$&',border=S:[screen],N=[screen]"'
79600   en if
79900 fn
80200 def fn_set_color(attribute$,foregroundDefault$,backgroundDefault$)
80500   foreground$=foregroundDefault$ ! fnureg_read('color.'&attribute$&'.foreground',foreground$) : if foreground$='' t foreground$=foregroundDefault$
80800   background$=backgroundDefault$ ! fnureg_read('color.'&attribute$&'.background',background$) : if background$='' t background$=backgroundDefault$
81100   exe 'Con Attr '&attribute$&' /'&foreground$&':'&background$ error ignore ! pr 'config attribute '&attribute$&' /'&foreground$&':'&background$ : pau
81400 fn
81700
82000 def fn_conSub(from$*256,to$*256; ___,quoteF$*1,quoteT$*1,returnN)
82300   if pos(from$,' ')>0 then quoteF$='"'
82600   if pos(to$,' ')>0 then quoteT$='"'
82900   ! pr 'config substitute '&quoteF$&from$&quoteF$&' '&quoteT$&to$&quoteT$
83200   exe 'config substitute '&quoteF$&from$&quoteF$&' '&quoteT$&to$&quoteT$
83500   returnN=1
83800   fn_conSub=returnN
84100 fnend
84400
84700 ErrorHandler: ! r:
85000   ! exec 'list -'&str$(line)
85300   pr bell;'---   ---   ---   ---   ---   ---   Error   ---   ---   ---   ---   ---   ---'
85600   exe 'st st'
85900   pr '        program: '&program$
86200   pr '      variable$: '&variable$
86500   pr '          error: '&str$(err)
86800   pr '          line: '&str$(line)
87100   exec 'list '&str$(line)
87400   pr bell;'---   ---   ---   ---   ---   ---   o-_-o   ---   ---   ---   ---   ---   ---'
87700   pau
88000 retry ! /r