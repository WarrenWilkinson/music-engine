/*!****************************************************************************
 * @file musicengine.c
 * @brief A shim between Alsa and Wireplumber and common lisp.
 *******************************************************************************/

#include <alsa/asoundlib.h>
#include <string.h>

#define MAX_SEQ 2
#define MAX_NAME_LEN 16
#define MAX_MIDI_PORTS 8

static int seq_used[MAX_SEQ];
static char seq_labels[MAX_SEQ][MAX_NAME_LEN];
static snd_seq_t* seq_handles[MAX_SEQ];

static int midi_port_used[MAX_SEQ][MAX_MIDI_PORTS];
static char midi_port_labels[MAX_SEQ][MAX_MIDI_PORTS][MAX_NAME_LEN];
static int midi_port_handles[MAX_SEQ][MAX_MIDI_PORTS];

int find_next_seq_slot(char** out_error_message) {
    for (int i = 0; i < MAX_SEQ; i++) {
        if (seq_used[i] == 0) {
            return i;
        }
    }

    *out_error_message = "No more seq slots available.";
    return -1;
}

int check_seq_number(int seq_number, char** out_error_message) {
    if (seq_number < 0) {
        *out_error_message = "seq_number was negative!";
        return -1;
    }
    if (seq_number >= MAX_SEQ) {
        *out_error_message = "seq_number was too large!";
        return -1;
    }
    if (seq_used[seq_number] == 0) {
        *out_error_message = "seq_number is not in use!";
        return -1;
    }
    return 0;
}

int check_midi_port_number(int seq_number, int midi_port_number, char** out_error_message) {
    int return_value = check_seq_number(seq_number, out_error_message);
    if (return_value < 0) {
        return -1;
    }
    
    if (midi_port_number < 0) {
        *out_error_message = "midi_port_number was negative!";
        return -1;
    }
    if (midi_port_number >= MAX_MIDI_PORTS) {
        *out_error_message = "midi_port_number was too large!";
        return -1;
    }
    if (midi_port_used[seq_number][midi_port_number] == 0) {
        *out_error_message = "midi_port_number was not in used!";
        return -1;
    }
    return 0;
}

int find_next_midi_port_slot(int seq_number, char** out_error_message) {
    int return_value = check_seq_number(seq_number, out_error_message);
    if (return_value < 0) {
        return -1;
    }
    for (int i = 0; i < MAX_MIDI_PORTS; i++) {
        if (midi_port_used[seq_number][i] == 0) {
            return i;
        }
    }

    *out_error_message = "No more midi port slots available.";
    return -1;
}

int check_name(char* name, char** out_error_message) {
    if (name == 0) {
        *out_error_message = "name was nil.";
        return -1;
    }

    if (strnlen(name, MAX_NAME_LEN - 1) == MAX_NAME_LEN - 1) {
        *out_error_message = "name was too long.";
        return -1;
    }

    return 0;
}

/*!****************************************************************************
 * @brief Creates a alsa seq device with a given name.
 * This function creates (and stores internally) an alsa sequencer and returns
 * identifying number (corresponding to the slot in which we stored it).
 * @param name the name of the sequencer (will be shown to users)
 * @param output set to 1 if this sequencer will output things
 * @param input set to 1 if this sequencer will input things
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_open_seq(char* name, int output, int input, char** out_error_message) {
    *out_error_message = "no problems so far.";
    int return_value = check_name(name, out_error_message);
    if (return_value < 0) {
        return -1;
    }

    int rw_mode = 0;
    if (input != 0 && output != 0) {
        rw_mode = SND_SEQ_OPEN_DUPLEX;
    } else if (input != 0) {
        rw_mode = SND_SEQ_OPEN_INPUT;
    } else if (output != 0) {
        rw_mode = SND_SEQ_OPEN_OUTPUT;
    } else {
        *out_error_message = "Both output and input are false";
        return -1;
    }
    
    int seq_number = find_next_seq_slot(out_error_message);
    if (seq_number < 0) {
        return -1;
    }
    
    return_value = snd_seq_open(&seq_handles[seq_number],
                                    "default", rw_mode, 0);

    if (return_value < 0) {
        *out_error_message = "Failure in snd_seq_open.";
        seq_handles[seq_number] = 0;
        seq_used[seq_number] = 0;
        memset(seq_labels[seq_number], 0, MAX_NAME_LEN);
        return -1;
    }

    // Mark the slot as used, keep a copy of the name, and set the name.
    seq_used[seq_number] = 1;
    strncpy(seq_labels[seq_number], name, MAX_NAME_LEN);
    return_value = snd_seq_set_client_name((snd_seq_t*)seq_handles[seq_number], seq_labels[seq_number]);

    if (return_value < 0) {
        *out_error_message = "Failure in snd_seq_set_client_name.";
        snd_seq_close((snd_seq_t*)seq_handles[seq_number]);
        seq_handles[seq_number] = 0;
        seq_used[seq_number] = 0;
        memset(seq_labels[seq_number], 0, MAX_NAME_LEN);
        return -1;
    }

    return seq_number;
}

int musicengine_open_midi_port(int seq_number, char* name, int output, int input, char** out_error_message) {
    *out_error_message = "no problems so far.";

    int return_value = check_seq_number(seq_number, out_error_message);
    if (return_value < 0) {
        return -1;
    }
    
    return_value = check_name(name, out_error_message);
    if (return_value < 0) {
        return -1;
    }

    if (input == 0 && output == 0) {
	*out_error_message = "Both output and input are false";
        return -1;
    }

    int flags = 0;
    if (input != 0) {
        flags |= SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE;
    }
    if (output != 0) {
	flags |= SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ;
    }

    int port_number = find_next_midi_port_slot(seq_number, out_error_message);
    if (port_number < 0) {
        return -1;
    }

    midi_port_used[seq_number][port_number] = 1;
    strncpy(midi_port_labels[seq_number][port_number], name, MAX_NAME_LEN);
    midi_port_handles[seq_number][port_number] =
	snd_seq_create_simple_port((snd_seq_t*)seq_handles[seq_number],
				   midi_port_labels[seq_number][port_number],
				   flags,
				   SND_SEQ_PORT_TYPE_APPLICATION);
    if (midi_port_handles[seq_number][port_number] < 0) {
	*out_error_message = "Failure in snd_seq_create_simple_port.";
	midi_port_handles[seq_number][port_number] = 0;
	midi_port_used[seq_number][port_number] = 1;
        memset(midi_port_labels[seq_number][port_number], 0, MAX_NAME_LEN);
	return -1;
    }

    return port_number;
}

/*!****************************************************************************
 * @brief Closes an alsa midi port.
 * This function closes an alsa midi_port.
 * @param seq_number
 * @param midi_number
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_close_midi_port(int seq_number, int midi_number, char** out_error_message) {
    *out_error_message = "no problems so far.";
    int return_value = check_midi_port_number(seq_number, midi_number, out_error_message);
    if (return_value < 0) {
        return -1;
    }

    return_value = snd_seq_delete_simple_port((snd_seq_t*)seq_handles[seq_number],
                                              midi_port_handles[seq_number][midi_number]);
    midi_port_handles[seq_number][midi_number] = 0;
    midi_port_used[seq_number][midi_number] = 0;
    memset(midi_port_labels[seq_number][midi_number], 0, MAX_NAME_LEN);
    return return_value;
}

/*!****************************************************************************
 * @brief Closes the alsa seq in the given slot. Closes open midi ports
 * This function closes the alsa sequencer; it'll also close any open midi ports.
 * @param seq_number
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_close_seq(int seq_number, char** out_error_message) {
    *out_error_message = "no problems so far.";
    int return_value = check_seq_number(seq_number, out_error_message);
    if (return_value < 0) {
        return -1;
    }

    // Close any open MIDI ports.
    for (int i = 0; i < MAX_MIDI_PORTS; i++) {
        if (midi_port_used[seq_number][i] != 0) {
            musicengine_close_midi_port(seq_number, i, out_error_message);
        }
    }

    return_value = snd_seq_close((snd_seq_t*)seq_handles[seq_number]);
    seq_handles[seq_number] = 0;
    seq_used[seq_number] = 0;
    memset(seq_labels[seq_number], 0, MAX_NAME_LEN);
    return return_value;
}

/*!****************************************************************************
 * @brief Initializes the music engine.
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_init(char** out_error_message) {
    *out_error_message = "no problems so far.";
    // Zero out all the static structures.
    for (int i = 0; i < MAX_SEQ; i++) {
        seq_used[i] = 0;
        memset(seq_labels[i], 0, MAX_NAME_LEN);
        seq_handles[i] = 0;
        for (int j = 0; j < MAX_MIDI_PORTS; j++) {
            midi_port_used[i][j] = 0;
            memset(midi_port_labels[i][j], 0, MAX_NAME_LEN);
            midi_port_handles[i][j] = 0;
        }
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
    *out_error_message = "no problems so far.";

    // Release all the resources
    for (int i = 0; i < MAX_SEQ; i++) {
        if (seq_used[i] != 0) {
	    musicengine_close_seq(i, out_error_message);
	}
    }

    return 0;
}
