# To use:
#
#     $ source env.sh

LLVM_18="/opt/homebrew/Cellar/llvm/18.1.8"

export MLIR_SYS_180_PREFIX="$LLVM_18"
export LLVM_SYS_180_PREFIX="$LLVM_18"

# Ensure llvm-config from LLVM 18 is visible on the path.
export PATH="$LLVM_18/bin/:$PATH"