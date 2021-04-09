library(eXSD)

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
  ),
  port=9855
)



