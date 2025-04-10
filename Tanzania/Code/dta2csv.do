clear
cd "~\radius\Tanzania\Data"

use "directed"
export delimited directed, replace

clear
use "dyadic"
export delimited dyadic, replace

clear
use "household"
export delimited household, replace

clear
use "illness"
export delimited illness, replace

clear
use "individual"
export delimited individual, replace

clear
use "panel"
export delimited panel, replace

clear
use "TransferPanel"
export delimited transfer_panel, replace