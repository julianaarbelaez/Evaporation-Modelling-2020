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
!! \brief: This module contains subroutines of boundary conditions for water flow
!<

  module REevapbc
  
    contains
  
  
    !> Evaporation rate [m/s]
    !> Input: Air Relative humiduty [-]
    function evaporation(layer, quadpnt) result(val)
      use typy
      use evapglob
      use evapextras
      use global_objs
        
      !>material ID  
      integer(kind=ikind), intent(in) :: layer
      !> Gauss quadrature point structure (element number and rank of Gauss quadrature point)
      type(integpnt_str), intent(in), optional :: quadpnt 
      !> Evaporation rate [m/s]
      real(kind=rkind) :: val
      !> Relative humidity soil
      !> liquid water density 
      !> saturated water vapor density
      real(kind=rkind) :: rh_soil_val, rho_l_val, rho_sv_val_soil, rho_sv_val_air
      real(kind=rkind), dimension(3,3) :: Ks
      
      type(integpnt_str) :: localpnt
        

      
      rh_soil_val = rh_soil(layer, quadpnt)
      rho_l_val = rho_l(quadpnt) 
      rho_sv_val_soil = rho_sv(quadpnt) 
      
      !air temperature
      localpnt%type_pnt = "numb"
      
      localpnt%this_is_the_value = temp_air
      
      rho_sv_val_air = rho_sv(localpnt) 
            
      val = (rh_soil_val*rho_sv_val_soil  - (rel_air_hum/100.0_rkind)* rho_sv_val_air )/(resistance*rho_l_val)
      
      val = max(val, 0.0_rkind)
    
    end function evaporation
    
    
    subroutine evap4bc(pde_loc, el_id, node_order, value, code, array) 
      use typy
      use globals
      use global_objs
      use pde_objs

           
      class(pde_str), intent(in) :: pde_loc
      integer(kind=ikind), intent(in)  :: el_id, node_order
      real(kind=rkind), intent(out), optional    :: value
      integer(kind=ikind), intent(out), optional :: code
      !> unused for this model (implementation for Robin boundary)
      real(kind=rkind), dimension(:), intent(out), optional :: array
      type(integpnt_str) :: quadpnt_loc
      real(kind=rkind), dimension(3) :: gravflux
      real(kind=rkind), dimension(3,3) :: K
      integer(kind=ikind) :: i, edge_id, j
      integer(kind=ikind) :: layer
      
      if (present(value)) then
        quadpnt_loc%column = 2
        quadpnt_loc%type_pnt = "ndpt"
        quadpnt_loc%order = elements%data(el_id,node_order)
        layer = elements%material(el_id)
        value = evaporation(layer, quadpnt_loc)
        call pde_loc%pde_fnc(pde_loc%order)%dispersion(pde_loc, elements%material(el_id), quadpnt_loc, &
                  tensor=K(1:drutes_config%dimen, 1:drutes_config%dimen))

        gravflux(1:drutes_config%dimen) = K(drutes_config%dimen, 1:drutes_config%dimen)*elements%nvect_z(el_id, node_order)
        value = -value-gravflux(1)

      end if
      
      if (present(code)) then
        code = 2
      end if
      
    end subroutine evap4bc
    
  end module REevapbc
