    Type :: typFieldMesh

    private

        !! Number of Node and Panel
        integer, public :: nNode, nPanel

        !! - Node Position
        type(typPoint), allocatable :: node(:)

        !! - Panel Connectivity
        type(typPanel), allocatable :: panel(:)

        !! - n Point Data
        integer, public :: nPointData

        !! - Point Data to write
        type(typPointDataVTK), allocatable, public :: pointData(:)

    End Type
