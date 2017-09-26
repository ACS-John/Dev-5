00010 ! Replace S:\Core\Process.br
00020 ! fnprocess is an value returning function.  It returns a 1=on, 0=off !:
        ! used for payroll and general ledger automatic processing
00030   def library fnprocess(; putpro)
00040     library 'S:\Core\Library': fncno
00050     option retain 
00060 ! ______________________________________________________________________
00070     let fncno(cno)
00080     if putpro=-1 then let process=oldpro=putpro=0
00090     if putpro>0 then let process=oldpro=putpro else let process=oldpro
00100 XIT: ! 
00110     let fnprocess=process
00120   fnend 
00130 ! ______________________________________________________________________
