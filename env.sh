# To use:
#
#     $ source env.sh

LLVM_18="/opt/homebrew/Cellar/llvm/18.1.8"

export MLIR_SYS_180_PREFIX="$LLVM_18"    # Needed to build `melior`
export LLVM_SYS_180_PREFIX="$LLVM_18"    # Needed to build `inkwell`

# Ensure llvm-config from LLVM 18 is visible on the path.
export PATH="$LLVM_18/bin/:$PATH"

# Note:
#   Might be useful in the future, I had to set it at one point but don't
#   remember why:
#
#   $ echo $SDKROOT
#   /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk
