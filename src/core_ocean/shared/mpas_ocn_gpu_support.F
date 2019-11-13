module ocn_gpu_support

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_constants

   implicit none
   private


   !--------------------------------------------------------------------
   !
   ! Public parameters
   !
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   !
   ! Public member functions
   !
   !--------------------------------------------------------------------

   public :: &
   			ocn_gpu_copy_mesh_to_device, ocn_gpu_post_core_init, &
   			ocn_gpu_delete_mesh_from_device

contains

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_gpu_copy_mesh_to_device
!
!
!-----------------------------------------------------------------------
   subroutine ocn_gpu_copy_mesh_to_device(domain)!{{{

      type(domain_type), intent(inout) :: &
         domain

      type(block_type), pointer :: &
         block

      type (mpas_pool_type), pointer :: meshPool

      integer, dimension(:), pointer :: nEdgesArray, nCellsArray, nEdgesOnEdge, &
      									nEdgesOnCell, maxLevelCell, maxLevelEdgeTop
      integer, dimension(:,:), pointer :: edgesOnEdge, cellsOnEdge, verticesOnEdge, cellsOnCell, &
      									edgesOnCell, edgeMask, verticesOnCell, edgeSignOnCell
      real (kind=RKIND), dimension(:), pointer :: fEdge, dcEdge, dvEdge, areaCell
      real (kind=RKIND), dimension(:,:), pointer :: weightsOnEdge

	  block => domain % blocklist
      do while ( associated(block) )
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
           
         call mpas_pool_get_array(meshPool, 'nEdgesOnEdge', nEdgesOnEdge)
         call mpas_pool_get_array(meshPool, 'maxLevelEdgeTop', maxLevelEdgeTop)
         call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
         call mpas_pool_get_array(meshPool, 'edgesOnEdge', edgesOnEdge)
	     call mpas_pool_get_array(meshPool, 'edgesOnCell', edgesOnCell)
	     call mpas_pool_get_array(meshPool, 'edgeSignOnCell', edgeSignOnCell)
         call mpas_pool_get_array(meshPool, 'edgeMask', edgeMask)
         call mpas_pool_get_array(meshPool, 'cellsOnCell', cellsOnCell)
         call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
         call mpas_pool_get_array(meshPool, 'weightsOnEdge', weightsOnEdge)
   	     call mpas_pool_get_array(meshPool, 'fEdge', fEdge)
	     call mpas_pool_get_array(meshPool, 'dcEdge', dcEdge)
         call mpas_pool_get_array(meshPool, 'dvEdge', dvEdge)
	     call mpas_pool_get_array(meshPool, 'areaCell', areaCell)
         call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
         call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
         call mpas_pool_get_array(meshPool, 'verticesOnCell', verticesOnCell)
           
         !$acc enter data copyin(nEdgesOnEdge, maxLevelEdgeTop, nEdgesOnCell, edgesOnEdge, &
         !$acc                   edgesOnCell, edgeSignOnCell, edgeMask, cellsOnCell, maxLevelCell, &
         !$acc                   weightsOnEdge,fEdge,dcEdge,dvEdge,areaCell,cellsOnEdge, &
         !$acc                   verticesOnEdge,verticesOnCell) async(1)
            
         block => block % next
      end do
 	  
   end subroutine ocn_gpu_copy_mesh_to_device

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_gpu_delete_mesh_from_device
!
!
!-----------------------------------------------------------------------
   subroutine ocn_gpu_delete_mesh_from_device(domain)!{{{

      type(domain_type), intent(inout) :: &
         domain

      type(block_type), pointer :: &
         block

      type (mpas_pool_type), pointer :: meshPool

      integer, dimension(:), pointer :: nEdgesArray, nCellsArray, nEdgesOnEdge, &
      									nEdgesOnCell, maxLevelCell, maxLevelEdgeTop
      integer, dimension(:,:), pointer :: edgesOnEdge, cellsOnEdge, verticesOnEdge, cellsOnCell, &
      									edgesOnCell, edgeMask, verticesOnCell, edgeSignOnCell
      real (kind=RKIND), dimension(:), pointer :: fEdge, dcEdge, dvEdge, areaCell
      real (kind=RKIND), dimension(:,:), pointer :: weightsOnEdge

	  block => domain % blocklist
      do while ( associated(block) )
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
           
         call mpas_pool_get_array(meshPool, 'nEdgesOnEdge', nEdgesOnEdge)
         call mpas_pool_get_array(meshPool, 'maxLevelEdgeTop', maxLevelEdgeTop)
         call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
         call mpas_pool_get_array(meshPool, 'edgesOnEdge', edgesOnEdge)
	     call mpas_pool_get_array(meshPool, 'edgesOnCell', edgesOnCell)
	     call mpas_pool_get_array(meshPool, 'edgeSignOnCell', edgeSignOnCell)
         call mpas_pool_get_array(meshPool, 'edgeMask', edgeMask)
         call mpas_pool_get_array(meshPool, 'cellsOnCell', cellsOnCell)
         call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
         call mpas_pool_get_array(meshPool, 'weightsOnEdge', weightsOnEdge)
   	     call mpas_pool_get_array(meshPool, 'fEdge', fEdge)
	     call mpas_pool_get_array(meshPool, 'dcEdge', dcEdge)
         call mpas_pool_get_array(meshPool, 'dvEdge', dvEdge)
	     call mpas_pool_get_array(meshPool, 'areaCell', areaCell)
         call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
         call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
         call mpas_pool_get_array(meshPool, 'verticesOnCell', verticesOnCell)
           
         !$acc exit data delete(nEdgesOnEdge, maxLevelEdgeTop, nEdgesOnCell, edgesOnEdge, &
         !$acc                   edgesOnCell, edgeSignOnCell, edgeMask, cellsOnCell, maxLevelCell, &
         !$acc                   weightsOnEdge,fEdge,dcEdge,dvEdge,areaCell,cellsOnEdge, &
         !$acc                   verticesOnEdge,verticesOnCell) async(1)
            
         block => block % next
      end do
 	  
   end subroutine ocn_gpu_delete_mesh_from_device

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_gpu_post_core_init
!
!
!-----------------------------------------------------------------------
   subroutine ocn_gpu_post_core_init(domain)!{{{

      type(domain_type), intent(inout) :: &
         domain

      type(block_type), pointer :: &
         block

      type (mpas_pool_type), pointer :: meshPool
      integer, dimension(:,:), pointer :: edgeSignOnCell, edgeSignOnCellNew
	    
	  block => domain % blocklist
      do while ( associated(block) )
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
         call mpas_pool_get_array(meshPool, 'edgeSignOnCell', edgeSignOnCell)
	     !$acc update device(edgeSignOnCell)           
         block => block % next
      end do

   end subroutine ocn_gpu_post_core_init
   
end module ocn_gpu_support
