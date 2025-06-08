# OASES

[OASES](https://tlo.mit.edu/technologies/oases-software-modeling-seismo-acoustic-propagation-horizontally-stratified-waveguides) is a general-purpose computer code developed by the [Massachusetts Institute of Technology](http://www.mit.edu/) for modeling seismo-acoustic propagation in horizontally stratified waveguides. It uses wavenumber integration combined with the Direct Global Matrix solution technique.

**I ran into many issues installing the base package on Windows, so I created a portable, Windows-friendly edition of OASES to make it easier for others.**

## Using OASES Portable Edition

For ease of use, you can run OASES 3.1 directly on Windows by using the portable edition. Follow these steps:

1. **Download** the portable edition from the release section. Choose the version corresponding to your operating system (32-bit or 64-bit).
   
2. **Extract** the archive to a folder of your choice.

3. **Update your PATH environment variable** to include the `bin` folder from the extracted folder. This allows you to run any OASES module directly from `cmd` or `PowerShell` (e.g., `oast`, `oasr`, etc.) using the syntax provided in the official documentation.

**Note**: The portable edition is built using Windows 11 with MSYS2 (mingw32 and mingw64) and GCC, see below.

**Warning**: This portable edition is compute-only. You won’t be able to plot results directly. To plot your results, use the functions provided in the `third_party` folder with MATLAB or Python.

_Last successful run: March 19, 2025_

## Building OASES Portable Edition (x64) on Windows 11 
You can build OASES using **MSYS2** on **Windows 11** with the 64-bit GCC and Fortran toolchain. 

### 1. Install MSYS2 and Required Tools 
- [Download MSYS2](https://www.msys2.org/) and install it. 
- Open the **MSYS2 MINGW64** shell. 
- Update the package manager and install required tools: 
```bash 
# First-time update (may require restarting shell) 
pacman -Syu 
# After restarting 
pacman -Su 
# Install 64-bit toolchain and build utilities 
pacman -S mingw-w64-x86_64-gcc \ 
          mingw-w64-x86_64-gcc-fortran \ 
          mingw-w64-x86_64-cmake \ 
          mingw-w64-x86_64-ninja 
``` 
Add the folder `C:\msys64\mingw64\bin` to your system PATH (for use in `cmd.exe`).

### 2. Clone and Build OASES 
Open a regular `cmd.exe` and run: 
```cmd 
:: Clone the repository 
git clone https://github.com/raphaelvdumas/OASES 
cd OASES 

:: Configure with Ninja and MinGW compilers 
cmake -G Ninja -B build -DCMAKE_C_COMPILER=gcc -DCMAKE_Fortran_COMPILER=gfortran 
:: Build and install 
cmake --build build 
cmake --install build 
``` 

### 3. Result 
- The executables and required DLLs will be placed (automatically) in the `./release/Oases-3.1-Windows64/bin` folder. 
- You can now copy the `Oases-3.1-Windows64` folder to any compatible system and run OASES by adding the `bin` folder to the environment variables.
