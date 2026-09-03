# Prospective Study 1/2 source freeze.
#
# IMPORTANT:
# - Run only after Phase 7A/7B validation is complete.
# - Run only after the exact final source state is committed and pushed.
# - The default approved protocol file must exist.
# - This creates/validates the source-freeze bundle only.
# - It does NOT run Study 1 or Study 2.
# - The definitive simulations remain blocked until the external prospective
#   registration is completed and record_study12_registration() is called.

library(devtools)
load_all()

prepare_study12_freeze()
