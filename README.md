# oases-public

[OASES](https://tlo.mit.edu/technologies/oases-software-modeling-seismo-acoustic-propagation-horizontally-stratified-waveguides) is a software package from [MIT](http://www.mit.edu/) for modeling seismo-acoustic propagation.

> **OASES (range-independent, publicly available)**  
> The [base package](https://oceanai.mit.edu/lamss/pmwiki/pmwiki.php?n=Site.Oases) uses wavenumber integration with the Direct Global Matrix solution technique in horizontally stratified waveguides for range-independent environments only.

> **C-SNAP (range-dependent, publicly available)**  
> Included in this installation, [C-SNAP](https://oceanai.mit.edu/lamss/pmwiki/pmwiki.php?n=Site.Csnap) uses normal mode theory to solve range-dependent acoustic field problems.

**I ran into many issues installing the base package on Windows and Ubuntu, so I created a friendly build of OASES to make it easier for others.**

## Roadmap

This table summarizes the support status for different components across platforms.

<table>
<tr>
  <td style="vertical-align: top; padding-right: 20px;">

| Component  | Linux 64-bit | Windows 64-bit |
|------------|:------------:|:--------------:|
| **oasi**   | ✅           | ✅             |
| **oasn**   | ✅           | ✅             |
| **oasp**   | ✅           | ✅             |
| **oasr**   | ✅           | ✅             |
| **oass**   | ✅           | ✅             |
| **oassp**  | ✅           | ✅             |
| **oast**   | ✅           | ✅             |
| **oasp3**  | ✅           | ✅             |
| **pp**     | ✅           | ✅             |
| **c-snap** | ✅           | ✅             |

  </td>
  <td style="vertical-align: top; padding-left: 20px;">

| Component   | Linux 64-bit | Windows 64-bit |
|-------------|:------------:|:--------------:|
| **mfp**    | ✅           | ✅             |
| **munkgen**| ✅           | ✅             |
| **nrmcov** | ✅           | ✅             |
| **addcov** | ✅           | ✅             |
| **coher**  | ✅           | ✅             |
| **trfsplit**| ✅          | ✅             |
| **trftoascii**| ✅        | ✅             |

  </td>
  <td style="vertical-align: top; padding-left: 20px;">

| Component   | Linux 64-bit | Windows 64-bit |
|-------------|:------------:|:--------------:|
| **saccon**   | ✅           | ❌             |
| **mintopost**| ✅           | ❌             |
| **mintops**  | ✅           | ❌             |
| **fipplot**  | ✅           | ❌             |
| **cplot**    | ✅           | ❌             |
| **mplot**    | ✅           | ❌             |
| **mtvplot**  | ✅           | ❌             |
| **multmtv**  | ✅           | ❌             |
| **plp2mtv**  | ✅           | ❌             |

  </td>
</tr>
</table>

Legend:  
- ✅ Supported  
- ❌ Not supported  

**Warning:** Errors may occur. If you encounter any issues, please report them in this repository.

**Note:** On Windows, this repository supports computation only — plotting is not supported due to X11 incompatibility with Windows (which will soon be deprecated with GNOME). Therefore, this version is strictly for computation.  

To visualize results, please use the MATLAB or Python functions available in the `third_party` folder.

## Building OASES (x64)

OASES can be built on **Windows 11** (using MSYS2) or **Ubuntu 24.04 (Noble Numbat)** with the 64-bit GCC and Fortran toolchain.

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
git clone https://github.com/raphaelvdumas/oases-public
cd oases-public

cmake -G Ninja -B build -DCMAKE_C_COMPILER=gcc -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build
cmake --install build
```
### 3. Add `bin` Folder to Your PATH

To run OASES modules from any location, add the `bin` folder to your system `PATH`. This enables running commands like `oast`, `oasr` (OASES), or `c-snap` (C-SNAP) directly from `cmd.exe` on Windows or the terminal on Ubuntu.

Refer to the official [OASES User Guide](https://github.com/raphaelvdumas/oases-public/blob/master/doc/OASES%203.1%20-%20User%20Guide.pdf) and [C-SNAP User Guide](https://github.com/raphaelvdumas/oases-public/blob/master/doc/C-SNAP%20-%20User%20Guide.pdf) for command details.

_Last successful build: June 2025_


## Notes on Building and Interoperability

> **Ubuntu Build Compatibility**  
> The project can be built on Ubuntu using all default plotting functions without modifications.

> **Replaced Unix Shells (Windows Compatibility)**  
> Previously included Unix-compatible shells in the `bin` folder (see [historical v3.1.0 release](https://github.com/raphaelvdumas/oases-public/releases/tag/v3.1.0-historical))  
> have been _replaced by a C script_ to ensure _Fortran-C interoperability_.

> **Environment Variable Management**  
> Environment variables are _automatically set_ by each C script (both Windows and Linux) to ensure the same output files as produced by the original Unix shells.

> **Fortran Refactoring for Interop**  
> Each Windows-compatible Fortran `program` has been _renamed to `subroutine`_ to support C interoperability.

> **Code Integrity Maintained**  
> No other modifications were made inside the core OASES source code itself.
