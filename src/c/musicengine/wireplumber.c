/*!****************************************************************************
 * @file wireplumber.c
 * @brief A shim between Wireplumber and common lisp.
 *******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <wp/wp.h>
#include <gobject/gobject.h>

// The way this works is as follows:
// Lisp will tell the shim the NAMES of the nodes it has an interest in.
// The C code will register an interest in nodes and maintain two arrays:
//
// 1. Lisp Name (String) -> int ID  (this one done by Lisp telling C)
// 2. int Id -> gpointer or null    (this one done by C talking with wireplumber).
//
// In addition, C will maintain a list of ports for these nodes, which
// can be returned to Lisp
// 3. int Id, Port Name (String), int port_id, int is_input

// Finally C will maintain a list of connections which can be returned to Lisp.
//
// 4. int output_port, int input_port
//
// Because of wireplumber this stuff should probably be atomic...
// so Lisp will send down a STRING of names (comma separated) and C will
// update table 1 with a mutex.
//
// C mostly ignores the signals from wireplumber, except that one about changes
// when it gets that it locks the mutex and updates tables 2 3 and 4
//
// When Lisp asks for state, C returns a string of information:
// found_names (comma separated) NEWLINE
// ports name:port_name IN/OUT MIDI/ANALOG (comma separated) NEWLINE
// edges name:port->name:port (comma separated) NEWLINE

#define MAX_NODE_NAME_LEN 64
#define MAX_NODE_NAMES 64

// Remember, actual max length less by 1 to support the null character.
static char node_interests[MAX_NODE_NAMES][MAX_NODE_NAME_LEN];
static int node_interests_len = 0;
static WpObjectManager *wp_object_manager = 0;
static WpCore *wp_core = 0;


void reset_node_interests() {
    memset(node_interests, 0, sizeof(char) * MAX_NODE_NAMES * MAX_NODE_NAME_LEN);
    node_interests_len = 0;
}


/*!****************************************************************************
 * @brief Tell wireplumber the names of nodes we have an interest in.
 * C will record these names and work with wireplumber to track them.
 * @param names comma separated list of names with null terminating character
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success or < 0 on error.
 *******************************************************************************/
int musicengine_register_node_interest(char* names, char** out_error_message) {

    int index = 0;
    char* name = strtok(names, ",");
    while (name != NULL) {

	// Ensure not too many names.
	if (index >= MAX_NODE_NAMES) {
	    *out_error_message = "too many node name interests.";
	    reset_node_interests();
	    // reset all other data also...
	    return -1;
	}

	// Ensure name isn't too long.
	if (strnlen(name, MAX_NODE_NAME_LEN) == MAX_NODE_NAME_LEN) {
	    *out_error_message = "name was too long;";
	    reset_node_interests();
	    // reset all other data also...
	    return -1;
	}

	// Ensure no spaces. (Actually spaces are totally fine)
	/* 
	if (strchr(name, ' ') != NULL) {
	    *out_error_message = "name contains a space";
	    reset_node_interests();
	    // reset all other data also...
	    return -1;
	}
	*/

	// Copy the name in and go to the next
	strncpy(node_interests[index], name, MAX_NODE_NAME_LEN);
        name = strtok(NULL, ",");
	index = index + 1;
    }

    node_interests_len = index;

    // reset all other data also and retrigger updating it.
    return 0;
}

/*!****************************************************************************
 * @brief Inquire as to the state of nodes and connections
 * C will return a three line string that summarizes the state of the nodes that
 * had a previous interest registered. The first line is a comma separated
 * list of node names.  The second line a comma separated list of ports
 * in the form <node_name>:<port_name> <midi_or_analog> <in_or_out>.
 * The third line is a comma separated list of edges in the form
 * <out_name>:<out_port>-><in_name>:<in_port>. The last character written (buffer size permitting) will be
 * the null termination character.
 * @param buffer A lisp allocated buffer where C can write the results.
 * @param buffer_size The size of the lisp allocated buffer.
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success or < 0 on error.
 *******************************************************************************/
int musicengine_get_node_state(char* buffer, int buffer_size, char** out_error_message) {
    // Write out all the names (should be right out ONLY the found names).
    for (int index = 0; index < node_interests_len; index++) {
	char* c = index == (node_interests_len - 1) ? "\n" : ",";
	int written = snprintf(buffer, buffer_size, "%s%s", node_interests[index], c);
	buffer+=written;
	buffer_size-=written;
    }

    // Write out the ports...
    {
	int had_data = 0;
	WpIterator *i = wp_object_manager_new_iterator(wp_object_manager);
	GValue item;// = NULL;
	while (wp_iterator_next(i, &item)) {
	    had_data = 1;
	    int written = snprintf(buffer, buffer_size, "%p,", &item);
	    buffer+=written;
	    buffer_size-=written;
	};
	if (had_data == 1) {
	    // backup a character to erase last comma
	    buffer-=1;
	    buffer_size+=1;
	}
	int written = snprintf(buffer, buffer_size, "\n");
	buffer+=written;
	buffer_size-=written;
    }

    // Write out the edges...
    {
	int written = snprintf(buffer, buffer_size, "\n");
	buffer+=written;
	buffer_size-=written;
    }

    /* // Write terminating 0 */
    /* { */
    /* 	int written = snprintf(buffer, buffer_size, "\0"); */
    /* 	buffer+=written; */
    /* 	buffer_size-=written; */
    /* } */

    if (buffer_size < 0) {
	*out_error_message = "buffer was too small.";
	return -1;
    }

    return 0;
}

/*!****************************************************************************
 * @brief Break a wireplumber connection
 * C will tell wireplumber to break the connection between the out_port and in_port
 * if it exists.
 * @param out_port The out port name (<node_name>:<port_name>). null terminated string.
 * @param in_port The in port name (<node_name>:<port_name>). null terminated string.
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.
 * @return returns >= 0 on success or < 0 on error.
 *******************************************************************************/
int musicengine_remove_edge(char* out_port, char* in_port, char** out_error_message) {
    return 0;
}

/*!****************************************************************************
 * @brief Add a wireplumber connection
 * C will tell wireplumber to create a connection between the out_port and in_port
 * if possible.
 * @param out_port The out port name (<node_name>:<port_name>). null terminated string.
 * @param in_port The in port name (<node_name>:<port_name>). null terminated string.
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.
 * @return returns >= 0 on success or < 0 on error.
 *******************************************************************************/
int musicengine_add_edge(char* out_port, char* in_port, char** out_error_message) {
    return 0;
}

/*!****************************************************************************
 * @brief Initializes the music engine.
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_wireplumber_init(char** out_error_message) {
    *out_error_message = "no problems so far.";

    memset(node_interests, 0, sizeof(char) * MAX_NODE_NAMES * MAX_NODE_NAME_LEN);
    node_interests_len = 0;

    wp_init(WP_INIT_ALL);

    GType wp_node_gtype = g_type_from_name("WpNode");
    if (wp_node_gtype == 0) {
	*out_error_message = "Failed to find WpNode gtype!";
	return -1;
    }

    wp_core = wp_core_new(NULL, NULL);
    if (wp_core == NULL) {
	*out_error_message = "Failed to create wp core object!";
	return -1;
    }

    wp_object_manager = wp_object_manager_new();
    if (wp_object_manager == NULL) {
	*out_error_message = "Failed to create wp object manager!";
	return -1;
    }
    
    wp_object_manager_add_interest (wp_object_manager, wp_node_gtype, NULL);
    wp_core_install_object_manager(wp_core, wp_object_manager);
    return 0;
}

/*!****************************************************************************
 * @brief Shutdown the musicengine
 * @param out_error_message is set by this function to return a string describing
 *                          problems to Lisp.          
 * @return returns >= 0 on success (the slot used) or < 0 on error.
 *******************************************************************************/
int musicengine_wireplumber_teardown(char** out_error_message) {
    *out_error_message = "no problems so far.";

    memset(node_interests, 0, sizeof(char) * MAX_NODE_NAMES * MAX_NODE_NAME_LEN);
    node_interests_len = 0;

    if (wp_core != NULL) {
	wp_core_disconnect(wp_core);
	wp_core = NULL;
	wp_object_manager = NULL;
    }
    
    return 0;
}
