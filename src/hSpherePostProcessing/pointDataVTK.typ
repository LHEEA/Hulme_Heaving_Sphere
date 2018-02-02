    Type, public :: typPointDataVTK

        Integer :: nPoint

        Logical :: isScalarData = .FALSE.

        Logical :: isVectorData = .FALSE.

        Real(RP), allocatable :: scalarData(:)

        Real(RP), allocatable :: vectorData(:, :)

    contains

        procedure, pass :: initialize => initializePointDataVTK

        procedure, pass :: setPointData => setVTKPointScalarData

        procedure, pass :: setVectorData => setVTKPointVectorData

        procedure, pass :: removeScalarData

        procedure, pass :: removeVectorData

    End Type
