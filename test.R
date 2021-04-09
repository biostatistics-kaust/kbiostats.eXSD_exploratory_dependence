source("./R_package_manager.R")

dependency.explorer(
  load_csv_datasets(
    path=system.file("TUH_EEG", package="eXSD"),
    filenames=list(
        SZ1="EEG715-seizure-01.csv",
        BK1="EEG715-plain-01.csv"
    ),
    length=200
  ),
  labels=list(
    SZ1="Seizure (segment 1)",
    BK1="Background (segment 1)"
  )
)

dependency.explorer(
  load_csv_datasets(
    path=system.file("TUH_EEG", package="eXSD"),
    filenames=list(
        SZ1="EEG715-seizure-01.csv",
        SZ2="EEG715-seizure-02.csv",
        SZ3="EEG715-seizure-03.csv",
        SZ4="EEG715-seizure-04.csv",
        SZ5="EEG715-seizure-05.csv",
        SZ6="EEG715-seizure-06.csv",
        BK1="EEG715-plain-01.csv",
        BK2="EEG715-plain-02.csv",
        BK3="EEG715-plain-03.csv",
        BK4="EEG715-plain-04.csv",
        BK5="EEG715-plain-05.csv",
        BK6="EEG715-plain-06.csv"
    ),
    length=200
  ),
    labels=list(
        SZ1="Seizure (segment 1)",
        SZ2="Seizure (segment 2)",
        SZ3="Seizure (segment 3)",
        SZ4="Seizure (segment 4)",
        SZ5="Seizure (segment 5)",
        SZ6="Seizure (segment 6)",
        BK1="Background (segment 1)",
        BK2="Background (segment 2)",
        BK3="Background (segment 3)",
        BK4="Background (segment 4)",
        BK5="Background (segment 5)",
        BK6="Background (segment 6)"
    )
)


