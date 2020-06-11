! Copyright 2008 Michal Kuraz, Petr Mayer, Copyright 2016  Michal Kuraz, Petr Mayer, Johanna Bloecher
! Copyright 2019  Michal Kuraz, Petr Mayer, Johanna Bloecher, Juliana Arbelaez
! Copyright 2020  Michal Kuraz, Petr Mayer, Johanna Bloecher, Juliana Arbelaez
! This file is part of DRUtES.
! DRUtES is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! DRUtES is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with DRUtES. If not, see <http://www.gnu.org/licenses/>.

!> \file: REevapbc.f90
!! \brief: This module contains subroutines for linking the PDEs
!<



module evappointers
  public :: REevap_proc, REevap_linker
  contains
  
    subroutine REevap_proc(processes) 
      use typy
      
      integer(kind=ikind), intent(out) :: processes
      
      processes = 2
    
    end subroutine REevap_proc
    
    subroutine REevap_linker()
      use typy
      use global_objs
      use pde_objs
      use globals
      use heat_pointers
      use re_pointers
      use evapglob
      use evap_heat_fnc
      use evap_RE_fnc
      use REevapbc
      use heatevapbc
      
      


      call heat(pde(:))

      pde(re_ord)%pde_fnc(re_ord)%dispersion => evapdiffhh
      
      pde(re_ord)%pde_fnc(heat_ord)%dispersion => evapdiffhT
      
      pde(re_ord)%pde_fnc(re_ord)%zerord  =>  dtheta_vdt

      pde(re_ord)%flux  =>  water_flux

      pde(heat_ord)%pde_fnc(heat_ord)%dispersion => evapdiffTT
      
      pde(heat_ord)%pde_fnc(re_ord)%dispersion => evapdiffTh
      
      pde(heat_ord)%pde_fnc(heat_ord)%convection => evap_heatconvect
      
      pde(heat_ord)%pde_fnc(heat_ord)%zerord  =>  Ldtheta_vdt
      
      
      pde(re_ord)%bc(102)%value_fnc => evap4bc
      
      pde(heat_ord)%bc(102)%value_fnc => soil_heat_flux
      
      
          
    end subroutine REevap_linker
  

  


end module evappointers
