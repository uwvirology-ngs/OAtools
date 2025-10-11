# basilisk environment configuration
OAtools_env <- basilisk::BasiliskEnvironment(
    envname = "conda_env",
    pkgname = "OAtools",
    packages = c(
        "python=3.13.5", 
        "numpy=2.3.2", 
        "pandas=2.3.1", 
        "scipy=1.16.1"
    ),
    channels = c("conda-forge")
)
