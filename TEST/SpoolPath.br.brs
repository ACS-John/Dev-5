20020 Execute "config gui off"
20040 library 'S:\Core\Library': fnspoolpath$
20060 on error goto ERTN
20080 Let A=1
20100 do
20120   Open #20: "name=pdf:/,recl=512,replace",display,output
20140   Print #20: STR$(A)
20160   Close #20: ioerr ignore
20180   Print STR$(A)
20200   A+=1
20220 loop while A<500
20240 XIT: end
20260 ERTN: ! r:
20280 if err=4261 then
20300   execute 'free '&fnspoolpath$&'\*.*'
20320   retry
20340 else
20360   pr 'error '&str$(err)&' on line '&str$(line)
20380   pause
20400 end if
20420 ! /r