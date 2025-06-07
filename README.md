# OASES

[OASES](https://tlo.mit.edu/technologies/oases-software-modeling-seismo-acoustic-propagation-horizontally-stratified-waveguides) is a general-purpose computer code developed by the [Massachusetts Institute of Technology](http://www.mit.edu/) for modeling seismo-acoustic propagation in horizontally stratified waveguides. It uses wavenumber integration combined with the Direct Global Matrix solution technique.

**I had a lot of trouble installing this base package on Windows, so I decided to create a friendly installation guide to help others.**

## Using OASES Portable Edition

For ease of use, you can run OASES 3.1 directly on Windows by using the portable edition. Follow these steps:

1. **Download** the portable edition from the release section. Choose the version corresponding to your operating system (32-bit or 64-bit).
   
2. **Extract** the archive to a folder of your choice.

3. **Update your PATH environment variable** to include the `bin` folder from the extracted folder. This allows you to run any OASES module directly from `cmd` or `PowerShell` (e.g., `oast`, `oasr`, etc.) using the syntax provided in the official documentation.

**Note**: The portable edition is built using Windows 11 with MSYS2 (mingw32 and mingw64) and GCC, see below.

**Warning**: This portable edition is compute-only. You won’t be able to plot results directly. To plot your results, use the functions provided in the `third_party` folder with MATLAB or Python.

_Last successful run: March 19, 2025_

## Building OASES Portable Edition

You can build OASES using **MSYS2** and **Ninja** on **Windows 11 (x86_64 or i686)**.

### 1. Prerequisites
- [Download MSYS2](https://www.msys2.org/) and install it.
- Open the MSYS2 shell and install required tools:
```bash
# Update MSYS2 packages (do this twice)
pacman -Syu   # Then close and reopen shell
pacman -Su    # Then proceed

# Install 64-bit toolchain and build tools
pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gfortran \
          mingw-w64-x86_64-cmake mingw-w64-x86_64-ninja
```

For 32-bit builds, use:
```bash
pacman -S mingw-w64-i686-gcc mingw-w64-i686-gfortran \
          mingw-w64-i686-cmake mingw-w64-i686-ninja
```

### 2. Clone OASES Repository
```bash
git clone https://example.com/oases.git
cd oases
```
### 3. Build Instructions

#### For Windows 64-bit:
```bash
cmake -G Ninja -B build -DCMAKE_C_COMPILER=gcc -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build
cmake --install build
```

#### For Windows 32-bit:
```bash
cmake -G Ninja -B build -DCMAKE_C_COMPILER=i686-w64-mingw32-gcc \
                            -DCMAKE_Fortran_COMPILER=i686-w64-mingw32-gfortran
cmake --build build
cmake --install build
```
### 4. Output
After installation, a `release/` folder will contain the final executables with the DLL dependencies automatically copied.


