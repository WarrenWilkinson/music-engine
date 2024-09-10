


sudo apt-get -y install libspa-0.2-dev



# C2FFI (obsolete)

C2FFI is used to investigate the pipewire C librarys and autogenerate
some json files that can be distributed with the project.  Once those json
files are present c2ffi isn't needed to build the project.  If you need to
rebuild those json files, these are the instructions:

    git clone https://github.com/rpav/c2ffi.git
    ;; Get LLVM (version 18 at time of writing)
    sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
    sudo apt-get install llvm-18 clang-18 libclang-18-dev libclang-cpp18-de
    
    ;; Build it
    rm -rf build
    mkdir build
    cd build
    CXX=clang++-18 cmake ../
    make
    sudo make install

Thene delete the spec files and load the package. It'll regenerate them. You might like this debug output:
    
    (setf CFFI/C2FFI::*trace-c2ffi* t)
