def fnco_properties_btn_ok
  ! library 'S:\Core\Library': fnIniSet,fnIniWrite
  library 'S:\Core\Library': fnwrite_program_print_property
  ! r: if Landscape/Portrait, than switch height and width if necessary
    tmp_height=val(s$(sio_txtHeight))
    tmp_width=val(s$(sio_txtWidth))
    if s$(sio_cmbOrientation)='Landscape' then 
      if tmp_height>tmp_width then ! and it's taller than it is wide
        tmp_hold=tmp_height
        tmp_height=tmp_width : s$(sio_txtHeight)=str$(tmp_height)
        tmp_width=tmp_hold : s$(sio_txtWidth)=str$(tmp_width)
      end if 
    else if s$(sio_cmbOrientation)='Portrait' then
      if tmp_width>tmp_height then ! and it's wider than it is tall
        tmp_hold=tmp_height
        tmp_height=tmp_width : s$(sio_txtHeight)=str$(tmp_height)
        tmp_width=tmp_hold : s$(sio_txtWidth)=str$(tmp_width)
      end if 
    end if 
  ! /r
  fnwrite_program_print_property('Orientation',     s$(sio_cmbOrientation)    )
  fnwrite_program_print_property('Height',          s$(sio_txtHeight)         )
  fnwrite_program_print_property('Width',           s$(sio_txtWidth)          )
  fnwrite_program_print_property('Lines',           s$(sio_txtLpp)            )
  fnwrite_program_print_property('FontSize',        s$(sio_txtFontSize)       )
  fnwrite_program_print_property('TopMargin',       s$(sio_txtMarginTop)      )
  fnwrite_program_print_property('BottomMargin',    s$(sio_txtMarginBottom)   )
  fnwrite_program_print_property('LeftMargin',      s$(sio_txtMarginLeft)     )
  fnwrite_program_print_property('RightMargin',     s$(sio_txtMarginRight)    )
  ExitMode=QuitOnly
fnend
