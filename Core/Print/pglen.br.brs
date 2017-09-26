00010 ! Replace S:\Core\Print\PgLen.br
00020 ! determine current lines per page setting
00030   def library fnpglen(&pglen)
00050     library 'S:\Core\Library': fnread_program_print_property
00080     let fnread_program_print_property('Lines',lpp$) : let pglen=val(lpp$)
00100 fnend 
