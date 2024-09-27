/*!****************************************************************************
 * @file musicengine.c
 * @brief A shim between Alsa and common lisp.
 *******************************************************************************/

#include <alsa/asoundlib.h>
#include <string.h>

#include "alsa.h"
#include "wireplumber.h"

/*!****************************************************************************
 * @brief Initializes the music engine.
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_init(char** out_error_message) {
    *out_error_message = "no problems so far.";

    if (musicengine_alsa_init(out_error_message) < 0) {
	return -1;
    }

    if (musicengine_wireplumber_init(out_error_message) < 0) {
	return -1;
    }

    return 0;
}

/*!****************************************************************************
 * @brief Shutdown the musicengine
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_teardown(char** out_error_message) {
    // TODO make this also teardown the wireplumber stuff.
    *out_error_message = "no problems so far.";

    if (musicengine_alsa_teardown(out_error_message) < 0) {
	return -1;
    }

    if (musicengine_wireplumber_teardown(out_error_message) < 0) {
	return -1;
    }

    return 0;
}
