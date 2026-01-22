# basilisk environment configuration
OAtools_env <- basilisk::BasiliskEnvironment(
    envname = "conda_env",
    pkgname = "OAtools",
    packages = c(
        "python=3.12", 
        "numpy==2.4.1", 
        "pandas==2.3.3", 
        "scipy==1.17.0"
    ),
    channels = c("conda-forge")
)
