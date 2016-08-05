00010   library program$: fnunique_computer_id$
00012   setenv('Unique_Computer_ID','')
00020   print 'fn returns '&fnunique_computer_id$
00022   print 'env is '&env$('Unique_Computer_ID')
00030   end 
10000   def library fnunique_computer_id$*40
10020     if env$('Unique_Computer_ID')='' then 
10040       library 'R:\Core\library': fngethandle
10060       dim uci_tmp_filename$*512,tmp_line$*128,uuid$*36,hdd_serial$*36
10080       let uci_tmp_filename$='acs_uuid_tmp'&session$&'.txt'
10120       let hdd_serial$=''
10140       let uuid$=''
10160       execute 'sy -m wmic csproduct get UUID |more >"%temp%\'&uci_tmp_filename$&'"'
12080       open #h_tmp:=fngethandle: 'name=@:'&env$('client_temp')&'\'&uci_tmp_filename$&',EoL=None',display,input ! ioerr NO_WMIC
14000       linput #h_tmp: tmp_line$
14020       let tmp_line$=srep$(tmp_line$,chr$(10),'~')
14040       let tmp_line$=srep$(tmp_line$,chr$(13),'~')
14060       let tmp_line$=srep$(tmp_line$,' ','')
16000       do while pos(tmp_line$,'~~')>0
16020         let tmp_line$=srep$(tmp_line$,'~~','~')
16040       loop 
20000       if tmp_line$(1:4)<>'UUID' then 
20020 ! windows server 2003
20040         let tmp_pos_uuid=pos(tmp_line$,'UUID~')
20060         if tmp_pos_uuid>0 then 
20080           let tmp_line$(1:tmp_pos_uuid)=''
20100         else 
20120           print 'problem in fn_unique_computer_id$ - expected to say UUID' : pause 
20140         end if 
20160       end if 
20180 !     if tmp_line$(1:4)<>'UUID' then print 'problem in fn_unique_computer_id$ - expected to say UUID' : pause
20200       let uuid$=tmp_line$(6:len(tmp_line$)-1)
20220       close #h_tmp,free: 
24000       if uuid$='FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF' then 
24020         let uuid_valid=0
24040         execute 'sy -m wmic  DISKDRIVE get SerialNumber |more >'&uci_tmp_filename$ ioerr NO_WMIC
24060 !    execute 'sy -m wmic /output:"'&uci_tmp_filename$&'" DISKDRIVE get SerialNumber'   <--encoded in something other than ANSI, hard to read
24080         open #h_tmp:=fngethandle: 'name='&uci_tmp_filename$&',EoL=None',display,input ioerr NO_WMIC
24100         linput #h_tmp: tmp_line$
24120         let tmp_line$=srep$(tmp_line$,chr$(10),'~')
24140         let tmp_line$=srep$(tmp_line$,chr$(13),'~')
24160         let tmp_line$=srep$(tmp_line$,' ','')
24180         do while pos(tmp_line$,'~~')>0
24200           let tmp_line$=srep$(tmp_line$,'~~','~')
24220         loop 
24240         if tmp_line$(1:12)<>'SerialNumber' then print 'problem in fn_unique_computer_id$ - expected to say SerialNumber' : pause 
24260         let hdd_serial$=tmp_line$(14:len(tmp_line$)-1)
24280         close #h_tmp,free: 
24300       else 
24320         let uuid_valid=1
24340       end if 
24360       if uuid_valid then 
24380         let setenv('Unique_Computer_ID',uuid$)
24400       else 
24420         let setenv('Unique_Computer_ID',hdd_serial$)
24440       end if 
24460     end if 
24480     let fnunique_computer_id$=env$('Unique_Computer_ID')
24500 UCI_XIT: ! 
24520   fnend 
30000 NO_WMIC: ! r:
30020 ! windows XP does not support WMIC
30040   let setenv('Unique_Computer_ID',wsid$)
30060   let fnunique_computer_id$=env$('Unique_Computer_ID')
30080   goto UCI_XIT ! /r
