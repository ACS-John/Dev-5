01000 dim screenioScreenName$*256 ! use ScreenCode$ after the update and skip the proc.
15020 screenioScreenName$=ScreenCode$ ! env$('screenioSaveAndTestProgram') ! env$('screenioSaveAndTestProgram')  is set by calling proc: S:\Core\SSAT.prc
15040 if pos(screenioScreenName$,".") then let screenioScreenName$=screenioScreenName$(1:pos(screenioScreenName$,".")-1)
15060 if pos(screenioScreenName$,"\") then let screenioScreenName$=screenioScreenName$(pos(screenioScreenName$,"\",-1)+1:9999)
16000 ! pr 'screenioScreenName$="'&screenioScreenName$&'"' : pause
20000 ! program name SSAT - stands for ScreenIO Save And Test - but that field in ScreenIO was limited to 18 characters so this will have to do.
20020 library 's:\Core\Library.br': fnAcsSystemInitialize,fntop,fnfm
20040 fnAcsSystemInitialize(1)
20060 dim cap$*80
20080 fntop(program$,cap$:='ScreenIO Save And Test for '&parentkey$)
20100 ! r: debug for checking passed variables
20120 ! print 'parentkey$="'&parentkey$&'"'
20140 ! for passedDataItem=1 to udim(mat passedData$)
20160 !   pr 'passedData$('&str$(passedDataItem)&')="'&passedData$(passedDataItem)&'"'
20180 ! nex passedDataItem
20200 ! print 'parentkey$="'&parentkey$&'"'
20220 ! print 'key$="'&key$&'"'
20240 ! print 'record='&str$(record)
20260 ! print 'path$="'&path$&'"'
20280 ! pr program$
20300 ! pause
20320 ! /r
20340 fnfm(screenioScreenName$)
20360 exe 'system'

