# OASES

[OASES](https://tlo.mit.edu/technologies/oases-software-modeling-seismo-acoustic-propagation-horizontally-stratified-waveguides) is a general-purpose computer code developed by the [Massachusetts Institute of Technology](http://www.mit.edu/) for modeling seismo-acoustic propagation in horizontally stratified waveguides. It uses wavenumber integration combined with the Direct Global Matrix solution technique.

**I ran into many issues installing the base package on Windows, so I created a portable, Windows-friendly edition of OASES to make it easier for others.**

## Using OASES Portable Edition

For ease of use, you can run OASES 3.1 directly on Windows by using the portable edition. Follow these steps:

1. _Download_ the portable edition from the [release section](https://github.com/raphaelvdumas/OASES/releases/tag/v3.1.0). Choose the version corresponding to your operating system (32-bit or 64-bit).
   
2. _Extract_ the archive to a folder of your choice.

3. _Update your PATH environment variable_ to include the `bin` folder from the extracted folder. 

This allows you to run any OASES module directly from `cmd` or `PowerShell` (e.g., `oast`, `oasr`, etc.) using the syntax provided in the [official documentation](https://github.com/raphaelvdumas/OASES/blob/master/OASES%203.1%20-%20User%20Guide.pdf).

**Warning**: For Windows, this portable edition is compute-only. You won’t be able to plot results directly. To plot your results, use the functions provided in the `third_party` folder with MATLAB or Python.

_Last successful run: March, 2025_

## Building OASES Portable Edition (x64)

OASES can be built on **Windows 11** (using MSYS2) or **Ubuntu 24.04 (Noble Numbat)** with the 64-bit GCC and Fortran toolchain.

---

### 1. Install Required Tools

#### On Windows 11 (Using MSYS2)
1. [Download and install MSYS2](https://www.msys2.org/).
2. Open the **MSYS2 MINGW64** shell.
3. Run the following commands:

```bash
# Update package database and core tools
pacman -Syu   # If prompted, close and reopen the shell

# After restart, complete the update
pacman -Su

# Install the 64-bit toolchain and build utilities
pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-cmake mingw-w64-x86_64-ninja
```

4. Add the following path to your system environment variables (`PATH`), for use in `cmd.exe`:
```
C:\msys64\mingw64\bin
```

#### On Ubuntu 24.04
Open a terminal and run:
```bash
# Update package list and upgrade existing packages
sudo apt update && sudo apt upgrade -y

# Install build-essential tools, Fortran, CMake, Ninja, and X11 support
sudo apt install build-essential cmake git gfortran ninja-build libx11-dev
```

### 2. Clone and Build OASES
Run the following commands in your terminal (`cmd.exe` on Windows or the regular terminal on Ubuntu):
```bash
git clone https://github.com/raphaelvdumas/OASES
cd OASES

cmake -G Ninja -B build -DCMAKE_C_COMPILER=gcc -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build
cmake --install build
```

### 3. Result
- The compiled binaries and necessary libraries will be placed in the `./release/` folder.
- You can copy the `Oases-3.1-Windows64` or `Oases-3.1-Linux64` folder to another compatible system.
- To run OASES from anywhere, add the `bin` folder inside the release directory to your system `PATH`.

_Last successful build: June, 2025_

## Notes on Building and Interoperability

> **Ubuntu Build Compatibility**  
> The project can be built on Ubuntu using all default plotting functions without modifications.

> **Replaced Unix Shells (Windows Compatibility)**  
> Previously included Unix-compatible shells in the `bin` folder (see [historical v3.1.0 release](https://github.com/raphaelvdumas/OASES/releases/tag/v3.1.0-historical))  
> have been _replaced by a C script_ to ensure _Fortran-C interoperability_.

> **Environment Variable Management**  
> Environment variables are _automatically set_ by each C script (both Windows and Linux) to ensure the same output files as produced by the original Unix shells.

> **Fortran Refactoring for Interop**  
> Each Windows-compatible Fortran `program` has been _renamed to `subroutine`_ to support C interoperability.

> **Code Integrity Maintained**  
> No other modifications were made inside the core OASES source code itself.
