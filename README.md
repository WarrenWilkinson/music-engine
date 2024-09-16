# music-engine
An attempt to wire up pipewire with common-lisp and sooperlooper.

## Notes on AI

Based on clean input from my voice, and knowledge of my songs, an AI could be trained to determine the lyrics to display.

Similarly, based on intensity and the above, a selection of a visual could be made.

Finally a generic audio visualization could be selected and applied to the visual.

## Notes on the buttons

Tomorrow, maybe synthesize my notes out of this file and proceed with
trying to connect to wireplumber and ask it some questions.

Okay, actually maybe get the drawing inside my window. Figure out how to use cairo
and just draw everything in a single pass... that'll be fine.

The drawing library can actually be a CAIRO thing, so I can just set the music engine to some
state and draw it I guess... 


Need to connect to pipewire... NO!
Better to connect to wireplumber.. and possibly just via wpctl.

Yeah, on my computer, wireplumber is already running and with a low ID number.


For my device, I think I want some LEDs
1 for power, 1 for cpu is live, some for bootup stages, and some to display the state
of the various applications I use. For example one LED that goes green when pipewire
is running in the correct state.

So what apps will be running?  Sooperlooper for sure.


gstreamer?  some kind of video things?
 - A midi synthesizer
   - timidity?
   - FluidSynth (is probably the best)
   - GeonKick (for drums)
 - a programmed synthesizer 
   - csound?
   - sfizz? https://sfz.tools/sfizz/
   - Chuck
   - supercollider <-- probably this one?
   - https://github.com/Gwion/Gwion
   - Soundpipe? https://pbat.ch/proj/soundpipe.html  or sporth? https://pbat.ch/proj/cook/
Okay, so in the above, I think I probably just want supercollider for most of these things...

More syths... https://github.com/ad-si/awesome-music-production?tab=readme-ov-file#synthesizers

Guitarix?  Do I need this?  Probably.  Maybe not actually...


Okay, so probably I end up writing little soundpipe programs for the effects I want.
Some are attached to MIDI, some are attached to sound inputs. Some output sound, some
output events.

The problem is, for a reasonably complex sound (e.g. shoot the dog) there is a bunch of
audio sounds that make the back beat.   Plus drums.  I have one midi keyboard...

Okay, so I think I want to allow X many different percussive modes -- and have a switch for
quickly toggling them.   Some of these percussive modes are basically wingdings and others
are drum kits, and others are just midi keyboard sounds.  So a fast switch between four different
modes.  Two switches can do this because that gives me binary stuff...   Conceptuall it's bank0 or bank1
and mode0 or mode1.  Alternatively just notice the toggle and just have 4 switches, you just change the
switch to select the bank (or buttons).   Probably just 3 different banks is sufficient.  Maybe a bit
of both... I dunno.

Okay, so then there are effects... effects need to be applied to a "channel" like a specific bank or not.
Plus selecting the effects and determining if they are compatible.  Some customization might be nice too.

It might make sense to just say the customization applies to the LAST CHANNEL toggled.  But what about volume?
That would kind of suck...  Then there is sooperlooper... which needs to be controlled a lot... that I think
has banks.
MANY buttons for that to select which sooperlooper channel(s) and what not.  Plus all these controls for sooperlooper!


Okay so sooper-looper channels is a thing, and probably a rotary switch to change a bank... so I probably have like 8
tracks and a rotary thing to get access to even more tracks?  Or maybe just 8 tracks?  Okay, so 8 push buttons
to select tracks... they need to glow or something.   Then the record buttons are nearer to where my hands will be.

Each track will probably need an overwrite/overdub thing... I think overdub is the default and just have an erase button for
each one.  Also need MUTE and SOLO buttons. So this is a button pad of 8x (SELECT, MUTE, SOLO, ERASE).  Also a mute all
button. Record is going to record EVERYTHING that I'm playing... except vocals.  so there needs to be a vocal toggle button somewhere
which controls if that is captured or not.

I think I also want some kind of "merge up" button -- which merges the audio into the previous channel and erases the channel.
Then I can put a scratching pad that only scratches channel 0. 

I think each channel needs a volume control, and an overall volume control.
I also have drum pads -- these are near the pick-guard and are HARD WIRED to drum bank 0. Or a rotary switch. Generally
just drum bank 0.

Finally setup...
 How to select the percussion bank?  How to apply filters onto it?
 How to select the guitar effects?  This also has banks right?
 How to select the vocal effects?

So yeah, I have percussion banks... one set of filters applied to EACH BANK.
And vocal banks... one set of filters applied to each BANK.
And a set of guitar effects --- one set of filters applied to each BANK.
Basically per "instrument" -- there is the concept of a BANK selection and a filter chain.

After the "BANK" it's just audio. So the filter chains work with audio only.
The BANK can be multiple parts -- e.g. it CAN be analog to midi back to analog.
Banks are just numbered and are NOT customizable.

The filter chains can probably also just be numbered.  So to enter this stuff is a combination of numbers.
Probably something like this:
INSTRUMENT# - MODE# - BANK# (per instrument) - FILTER#

Instruments are 0 - vocal, 1 - midi, 2 - guitar, 3 - theramin
Mode is 0-3.
Bank# is probably less than 256.
Filter is probably less than 256.

So basically I just type them in...
But easier is probably to have a setup mode where these change.
then you just select the instrument + Mode by pressing the correct button.
Then use the 8x (SELECT, MUTE, SOLO, ERASE) = 32 buttons for sooperlooper twice to select bank and filter.
Then it should automatically play some sounds so I can hear what is selected.  Press a different instrument + mode (or the same to redo) to
go onto the next.  Press the setup mode button again.   This setup mode button might be good as a dipole switch to prevent accidental triggering.
Going into setup mode automatically clears sooperlooper, so I can just rock it twice to clear everything.

That's probably an OKAY setup.  One other thing... when you FIRST
switch to setup mode, until you actually press a setup button, the
32 buttons apply presets.  They can be complete (customizing every
bank) or partial (only affecting some banks). So generally you're
probably going to just do that to do the majority of assignments,
and then just customize a bank or two for a particular song.

Buttons like this:
https://www.adafruit.com/product/3350

About $10 each.  a $5 one doesn't let you change the color.
I probably don't need the color changing... but anyway there are like 40 of them, so $400.

Here is a square one about $4 so $140.
https://www.adafruit.com/product/491

These are 2.50 .. so $80.
https://www.adafruit.com/product/3430

Also an SD card... when an SD card is present I want to record
All the raw signals.
All the output signals.

In order to give me a reference of the performance so I can experiment with new setups.


A couple more buttons
Intense switch... basically when toggled all things become more intense.
this means all the filters basically toggle to a parallel filter that has
more amplification, echo, reverb, etc.  Intended for Chorus's and some songs
that become intense at the end.

A "tuning" toggle.  C, D, E, etc.  Basically serves as a hint for Midi conversion
and any vocoder effects that are producing MIDI from a signal.

Also tuning lights on the guitar itself... a built in tuner.  I'd also like
a Guitar tuning option... works with Midi or raw signal... Drop-D for example
performs a drop D conversion in software. 

THIS and wireplumber might be better:
https://github.com/andy128k/cl-gobject-introspection

This might be nicer interface wise...
https://github.com/bohonghuang/cl-gobject-introspection-wrapper

Or do it via Lua... but I don't think I want that... maybe I do. I dunno.
Okay, I think I have a solution for that... now then, lets spawn stuff up I guess?
is that an automatic thing or something my stuff handles?
I think probably systemd should handle that... maybe?  Maybe not?  Probably.
My device should sit there and look for it...

So lets try to do that first... lets try to make a Lisp program that is notified
when SooperLooper comes up.

## Garbage Notes about gobject

;; (gir:function-info-get-symbol (car (last (gir:repository-get-infos nil "cairo"))))
;; "cairo_image_surface_create"
;; (gir:nget *cairo* "image_surface_create") that works...
;; (gir:nget *cairo* "Context")

;; This one returns nothing...
;; (gir:struct-info-get-methods
;; 	  (slot-value (gir:nget *cairo* "Context")   'gir::info))
;;
;; This one returns a single method... 
;;(gir:struct-info-get-methods
;;       (slot-value (gir:nget *gtk* "ApplicationWindow")   'gir::info))

;; (mapcar #'gir:registered-type-info-get-type-name
;; 		 (gir:repository-get-infos nil "cairo"))
;; ("CairoContext" "CairoDevice" "CairoSurface" NIL "CairoPattern" "CairoRegion"
;;  "cairo_status_t" "cairo_content_t" "cairo_operator_t" "cairo_antialias_t"
;;  "cairo_fill_rule_t" "cairo_line_cap_t" "cairo_line_join_t"
;;  "cairo_text_cluster_flags_t" "cairo_font_slant_t" "cairo_font_weight_t"
;;  "cairo_subpixel_order_t" "cairo_hint_style_t" "cairo_hint_metrics_t"
;;  "CairoFontOptions" "cairo_font_type_t" "cairo_path_data_type_t"
;;  "cairo_device_type_t" "cairo_surface_type_t" "cairo_format_t"
;;  "cairo_pattern_type_t" "cairo_extend_t" "cairo_filter_t"
;;  "cairo_region_overlap_t" "CairoFontFace" "CairoScaledFont" NIL
;;  "CairoRectangle" "CairoRectangleInt" NIL)

;; (find "GtkApplicationWindow" (gir:repository-get-infos nil "Gtk") :key #'gir:registered-type-info-get-type-name :test #'string-equal)


;; (first (gir:repository-get-infos nil "cairo"))