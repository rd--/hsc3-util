#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "c-common/bits.h"
#include "c-common/network.h"
#include "c-common/observe-signal.h"
#include "c-common/osc.h"
#include "c-common/print.h"

#include "sensel.h"
#include "sensel_device.h"

void sensel_handle_error(SenselStatus err) {
    if(err != SENSEL_OK) {
        printf("sensel_handle_error: NOT SENSEL_OK");
        exit(1);
    }
}

const char *sensel_scan_detail_string[] = {"high", "medium", "low", "unknown"};
const char *sensel_scan_mode_string[] = {"disable", "sync", "async"};
const char *sensel_contact_state_string[] = {"invalid", "start", "move", "end"};

void sensel_print_settings(SENSEL_HANDLE sensel) {
    printf("SenselSettings\n");
    SenselScanDetail scanDetail;
    sensel_handle_error(senselGetScanDetail(sensel, &scanDetail));
    printf(" ScanDetail : enum = %s\n", sensel_scan_detail_string[scanDetail]);
    unsigned char num_leds;
    sensel_handle_error(senselGetNumAvailableLEDs(sensel, &num_leds));
    printf(" NumAvailableLEDs : u8 = %u\n", num_leds);
    unsigned short max_brightness;
    sensel_handle_error(senselGetMaxLEDBrightness(sensel, &max_brightness));
    printf(" MaxLEDBrightness : u16 = %hu\n", max_brightness);
    unsigned char powerButtonPressed;
    sensel_handle_error(senselGetPowerButtonPressed(sensel, &powerButtonPressed));
    printf(" PowerButtonPressed : bool = %s\n", powerButtonPressed ? "true" : "false");
    unsigned char contactsEnableBlobMerge;
    sensel_handle_error(senselGetContactsEnableBlobMerge(sensel, &contactsEnableBlobMerge));
    printf(" ContactsEnableBlobMerge : bool = %s\n", contactsEnableBlobMerge ? "true" : "false");
    unsigned short contactsMinForce;
    sensel_handle_error(senselGetContactsMinForce(sensel, &contactsMinForce));
    printf(" ContactsMinForce : u16 = %hu\n", contactsMinForce);
    unsigned char dynamicBaselineEnabled;
    sensel_handle_error(senselGetDynamicBaselineEnabled(sensel, &dynamicBaselineEnabled));
    printf(" DynamicBaselineEnabled : bool = %s\n", dynamicBaselineEnabled ? "true" : "false");
    unsigned char bufferControl;
    sensel_handle_error(senselGetBufferControl(sensel, &bufferControl));
    printf(" BufferControl : u8 = %u\n", bufferControl);
    SenselScanMode scanMode;
    sensel_handle_error(senselGetScanMode(sensel, &scanMode));
    printf(" ScanMode : enum = %s\n", sensel_scan_mode_string[scanMode]);
    unsigned short maxFrameRate;
    sensel_handle_error(senselGetMaxFrameRate(sensel, &maxFrameRate));
    printf(" MaxFrameRate : u16 = %hu\n", maxFrameRate);
    unsigned char contactsMask;
    sensel_handle_error(senselGetContactsMask(sensel, &contactsMask));
    printf(" ContactsMask : u8 = %u\n", contactsMask);
}

void sensel_print_device_info(SenselDeviceList *device_list, int i) {
    printf("device_number = %i\n", i);
    printf(" device_list->devices[%i]\n", i);
    printf("  .idx : u8 = %u\n", device_list->devices[i].idx);
    printf("  .serial_num : string = %s\n", device_list->devices[i].serial_num);
    SENSEL_HANDLE sensel = NULL;
    senselOpenDeviceByID(&sensel, device_list->devices[i].idx);
    SenselFirmwareInfo fw_info;
    senselGetFirmwareInfo(sensel, &fw_info);
    printf(" FirmwareInfo\n");
    printf("  .fw_protocol_version : u8 = %u\n", fw_info.fw_protocol_version);
    printf("  .fw_version_major : u8 = %u\n", fw_info.fw_version_major);
    printf("  .fw_version_minor : u8 = %u\n", fw_info.fw_version_minor);
    printf("  .fw_version_build : u16 = %hu\n", fw_info.fw_version_build);
    printf("  .fw_version_release : u8 = %u\n", fw_info.fw_version_release);
    printf("  .device_id : u16 = %hu\n", fw_info.device_id);
    printf("  .device_revision : u8 = %u\n", fw_info.device_revision);
    SenselSensorInfo sensor_info;
    senselGetSensorInfo(sensel, &sensor_info);
    printf(" SensorInfo\n");
    printf("  .max_contacts : u8 = %u\n", sensor_info.max_contacts);
    printf("  .num_rows : u16 = %hu\n", sensor_info.num_rows);
    printf("  .num_cols : u16 = %hu\n", sensor_info.num_cols);
    printf("  .width : f32 = %f (mm)\n", sensor_info.width);
    printf("  .height : f32 = %f (mm)\n", sensor_info.height);
    senselClose(sensel);
}

void sensel_print_contacts(void) {
    SENSEL_HANDLE sensel = NULL;
    senselOpen(&sensel);
    senselSetFrameContent(sensel, FRAME_CONTENT_CONTACTS_MASK);
    senselSetContactsMask(sensel, CONTACT_MASK_ELLIPSE);
    SenselFrameData *frame = NULL;
    senselAllocateFrameData(sensel, &frame);
    senselStartScanning(sensel);
    while(observe_end_of_process() == false) {
        sensel_handle_error(senselReadSensor(sensel));
        unsigned int num_frames = 0;
        sensel_handle_error(senselGetNumAvailableFrames(sensel, &num_frames));
        if(num_frames != 1) {
            printf("num_frames : u32 = %u\n", num_frames);
        }
        for (int f = 0; f < num_frames; f++) {
            sensel_handle_error(senselGetFrame(sensel, frame));
            if (frame->n_contacts > 0) {
                printf("frame->content_bit_mask : u8 = ");
                print_bit_mask_newline(stdout,frame->content_bit_mask);
                printf("frame->lost_frame_count : i32 = %i\n", frame->lost_frame_count);
                printf("frame->n_contacts : u8 = %hu\n", frame->n_contacts);
                if (frame->content_bit_mask & FRAME_CONTENT_CONTACTS_MASK) {
                    for (int c = 0; c < frame->n_contacts; c++) {
                        printf("frame->contacts[%d]\n", c);
                        printf(" .content_bit_mask : u8 = ");
                        print_bit_mask_newline(stdout,frame->contacts[c].content_bit_mask);
                        printf(" .id : u8 = %u\n", frame->contacts[c].id);
                        unsigned int state = frame->contacts[c].state;
                        printf(" .state : u32 = %u (%s)\n", state, sensel_contact_state_string[state]);
                        printf(" .x_pos : f32 = %f (mm)\n", frame->contacts[c].x_pos);
                        printf(" .y_pos : f32 = %f (mm)\n", frame->contacts[c].y_pos);
                        printf(" .total_force : f32 = %f (gr)\n", frame->contacts[c].total_force);
                        printf(" .area : f32 = %f (#s)\n", frame->contacts[c].area);
                        if(frame->contacts[c].content_bit_mask & CONTACT_MASK_ELLIPSE) {
                            printf(" .orientation : f32 = %f (°)\n", frame->contacts[c].orientation);
                            printf(" .major_axis : f32 = %f (mm)\n", frame->contacts[c].major_axis);
                            printf(" .minor_axis : f32 = %f (mm)\n", frame->contacts[c].minor_axis);
                        }
                        if (state == CONTACT_START) {
                            senselSetLEDBrightness(sensel, frame->contacts[c].id, 100);
                        } else if (state == CONTACT_END) {
                            senselSetLEDBrightness(sensel, frame->contacts[c].id, 0);
                        }
                    }
                }
            }
	}
    }
    printf("\n");
    printf("sensel_print_contacts: STOP\n");
    senselStopScanning(sensel);
    senselFreeFrameData(sensel,frame);
    senselClose(sensel);
}

void sensel_print_total_force(void) {
    SENSEL_HANDLE sensel = NULL;
    senselOpen(&sensel);
    SenselSensorInfo sensor_info;
    senselGetSensorInfo(sensel, &sensor_info);
    senselSetFrameContent(sensel, FRAME_CONTENT_PRESSURE_MASK);
    SenselFrameData *frame = NULL;
    senselAllocateFrameData(sensel, &frame);
    senselStartScanning(sensel);
    while(!observe_end_of_process()) {
        senselReadSensor(sensel);
        unsigned int num_frames = 0;
        senselGetNumAvailableFrames(sensel, &num_frames);
        for (int f = 0; f < num_frames; f++) {
            senselGetFrame(sensel, frame);
            float total_force = 0;
            for (int i = 0; i < sensor_info.num_cols * sensor_info.num_rows; i++) {
                total_force = total_force + frame->force_array[i];
            }
            printf("sum(frame->force_array) : float = %14.8f\r", total_force); // CARRIAGE-RETURN
	}
    }
    senselStopScanning(sensel);
    senselFreeFrameData(sensel,frame);
    senselClose(sensel);
}

void sensel_print_accel_data(void) {
    SENSEL_HANDLE sensel = NULL;
    senselOpen(&sensel);
    SenselSensorInfo sensor_info;
    senselGetSensorInfo(sensel, &sensor_info);
    senselSetFrameContent(sensel, FRAME_CONTENT_ACCEL_MASK);
    SenselFrameData *frame = NULL;
    senselAllocateFrameData(sensel, &frame);
    senselStartScanning(sensel);
    while(!observe_end_of_process()) {
        senselReadSensor(sensel);
        unsigned int num_frames = 0;
        senselGetNumAvailableFrames(sensel, &num_frames);
        for (int f = 0; f < num_frames; f++) {
            senselGetFrame(sensel, frame);
            float x = (float)(frame->accel_data->x) / 16384.0;
            float y = (float)(frame->accel_data->y) / 16384.0;
            float z = (float)(frame->accel_data->z) / 16384.0;
            printf("accel_data: x=%6.3f y=%6.3f z=%6.3f\r", x, y, z); // CARRIAGE-RETURN
	}
    }
    senselStopScanning(sensel);
    senselFreeFrameData(sensel,frame);
    senselClose(sensel);
}

typedef struct {
    bool text_mode;
    bool print_devices;
    char hostname[HOST_NAME_MAX];
    uint16_t port;
    bool voice_assign;
    int k0;
    SenselScanDetail scan_detail;
    unsigned short scan_rate;
    unsigned char usr_ct_max;
    unsigned short contactsMinForce;
} sensel_usr_opt;

void sensel_usr_opt_default(sensel_usr_opt *opt) {
    opt->print_devices = false;
    opt->text_mode = false;
    opt->voice_assign = false;
    opt->usr_ct_max = 16;
    strncpy(opt->hostname,"localhost",HOST_NAME_MAX - 1);
    opt->port = 57110;
    opt->k0 = 13000;
    opt->scan_rate = 125;
    opt->scan_detail = SCAN_DETAIL_MEDIUM;
    opt->contactsMinForce = 24;
}

void sensel_usr_opt_usage(void) {
    sensel_usr_opt opt;
    sensel_usr_opt_default(&opt);
    printf("hsc3-sensel\n");
    printf("  -d      print device information (default=%s)\n", opt.print_devices ? "true" : "false");
    printf("  -f      set ContactsMinForce (default=%hu valid=[8,16,24...])\n", opt.contactsMinForce);
    printf("  -h      print help\n");
    printf("  -k INT  set k0 (default=%d)\n",opt.k0);
    printf("  -m INT  set number of monitored contacts (default=%u)\n",opt.usr_ct_max);
    printf("  -n STR  set hostname (default=%s)\n",opt.hostname);
    printf("  -p INT  set port number (default=%hu)\n",opt.port);
    printf("  -r INT  set scan rate (default=%hu)\n",opt.scan_rate);
    printf("  -t      set text output mode (default=%s)\n",opt.text_mode ? "true" : "false");
    printf("  -v      set voice assign mode (default=%s)\n",opt.voice_assign ? "true" : "false");
    printf("  -x      set scan detail to high (default=%s)\n",sensel_scan_detail_string[opt.scan_detail]);
    exit(0);
}

int sensel_usr_opt_parse(sensel_usr_opt *opt,int argc, char **argv) {
    int c;
    while ((c = getopt(argc, argv, "df:hk:m:n:p:r:tvx")) != -1) {
        switch (c) {
        case 'd':
            opt->print_devices = true;
            break;
        case 'f':
            opt->contactsMinForce = (unsigned short)strtol(optarg, NULL, 0);
            break;
        case 'h':
            return -1;
            break;
        case 'k':
            opt->k0 = (int)strtol(optarg, NULL, 0);
            break;
        case 'm':
            opt->usr_ct_max = (int)strtol(optarg, NULL, 0);
            break;
        case 'n':
            strncpy(opt->hostname,optarg,HOST_NAME_MAX - 1);
            break;
        case 'p':
            opt->port = (uint16_t)strtol(optarg, NULL, 0);
            break;
        case 'r':
            opt->scan_rate = (unsigned short)strtol(optarg, NULL, 0);
            break;
        case 't':
            opt->text_mode = true;
            break;
        case 'v':
            opt->voice_assign = true;
            break;
        case 'x':
            opt->scan_detail = SCAN_DETAIL_HIGH;
            break;
        }
    }
    return 0;
}

void sensel_send_osc(const sensel_usr_opt opt) {
    int fd = socket_udp(0);
    struct sockaddr_in addr;
    init_sockaddr_in(&addr, opt.hostname, opt.port);
    const int osc_buf_max = 256;
    uint8_t osc_buf[osc_buf_max];
    SENSEL_HANDLE sensel = NULL;
    senselOpen(&sensel);
    senselSetFrameContent(sensel, FRAME_CONTENT_CONTACTS_MASK);
    senselSetContactsMask(sensel, CONTACT_MASK_ELLIPSE);
    senselSetContactsMinForce(sensel, opt.contactsMinForce);
    printf(" ContactsMinForce : u16 = %hu\n", opt.contactsMinForce);
    SenselSensorInfo sensor_info;
    senselGetSensorInfo(sensel, &sensor_info);
    int ct_voice_id[sensor_info.max_contacts]; /* array mapping contact.id -> voice-id | -1 */
    uint64_t voice_frame_active[sensor_info.max_contacts]; /* array storing most recent active frames counter for each contact */
    int ct_max = (int) (opt.usr_ct_max < sensor_info.max_contacts ? opt.usr_ct_max : sensor_info.max_contacts);
    printf("usr_ct_max : u8 = %u\n", opt.usr_ct_max);
    printf("ct_max : u8 = %u\n", ct_max);
    for (int i = 0; i < ct_max; i++) {
        ct_voice_id[i] = -1;
        voice_frame_active[i] = 0;
    }
    senselSetScanDetail(sensel, opt.scan_detail);
    senselSetMaxFrameRate(sensel, opt.scan_rate);
    sensel_print_settings(sensel);
    SenselFrameData *frame = NULL;
    senselAllocateFrameData(sensel, &frame);
    senselStartScanning(sensel);
    uint64_t frame_counter = 0;
    while(!observe_end_of_process()) {
        sensel_handle_error(senselReadSensor(sensel));
        unsigned int num_frames = 0;
        sensel_handle_error(senselGetNumAvailableFrames(sensel, &num_frames));
        for (int f = 0; f < num_frames; f++) {
            sensel_handle_error(senselGetFrame(sensel, frame));
            frame_counter++;
            if (frame->lost_frame_count > 1) {
                printf("frame->lost_frame_count : i32 = %i\n", frame->lost_frame_count);
            }
            dprintf("frame->n_contacts=%hu\n", frame->n_contacts);
            if (frame->n_contacts > 0) {
                if (frame->content_bit_mask & FRAME_CONTENT_CONTACTS_MASK) {
                    for (int c = 0; c < frame->n_contacts && frame->contacts[c].id < ct_max; c++) {
                        unsigned int state = frame->contacts[c].state;
                        if (state == CONTACT_INVALID) {
                            printf("c=%d : INVALID\n", c);
                        } else {
                            if (state == CONTACT_START) {
                                /* assign voice-id */
                                if (opt.voice_assign) {
                                    uint64_t k = UINT64_MAX;
                                    for (int i = 0; i < ct_max; i++) {
                                        if (voice_frame_active[i] < k) {
                                            k = voice_frame_active[i];
                                            ct_voice_id[frame->contacts[c].id] = i;
                                        }
                                    }
                                } else {
                                    ct_voice_id[frame->contacts[c].id] = frame->contacts[c].id;
                                }
                            }
                            int voice_id = ct_voice_id[frame->contacts[c].id];
                            voice_frame_active[voice_id] = frame_counter;
                            /* k g x y z o rx ry */
                            int k = opt.k0 + (voice_id * 10);
                            float g = (state == CONTACT_START || state == CONTACT_MOVE) ? 1.0 : 0.0;
                            float x = frame->contacts[c].x_pos / sensor_info.width;
                            float y = 1.0 - (frame->contacts[c].y_pos / sensor_info.height);
                            float z = frame->contacts[c].total_force / 2048.0; /* 4096.0 8192.0 */
                            //float o = fmodf(frame->contacts[c].orientation + 540.0,360.0) / 360.0;
                            float o = frame->contacts[c].orientation / 360.0 + 0.5;
                            float rx = (frame->contacts[c].major_axis - 10.0) / 16.0;
                            float ry = (frame->contacts[c].minor_axis - 10.0) / 6.0;
                            float p = x * 24 + 48;
                            dprintf("/c_setn k=%d 8 g=%f x=%f y=%f z=%f o=%f rx=%f ry=%f\n", k, g, x, y, z, o, rx, ry);
                            int osc_msg_sz = osc_build_message(osc_buf, k, "/c_setn", ",iiffffffff", k, 8, g, x, y, z, o, rx, ry, p);
                            sendto_exactly(fd, osc_buf, osc_msg_sz, addr);
                            if (state == CONTACT_START) {
                                senselSetLEDBrightness(sensel, voice_id, 100);
                            } else if (state == CONTACT_END) {
                                senselSetLEDBrightness(sensel, voice_id, 0);
                            }
                        }
                    }
                }
            }
	}
    }
    printf("\n");
    printf("sensel_send_osc: STOP\n");
    senselStopScanning(sensel);
    senselFreeFrameData(sensel,frame);
    senselClose(sensel);
    close(fd);
}

int main(int argc, char **argv) {
    sensel_usr_opt opt;
    sensel_usr_opt_default(&opt);
    if(sensel_usr_opt_parse(&opt,argc,argv) == -1) {
        sensel_usr_opt_usage();
    }
    observe_signals();
    SenselDeviceList device_list;
    senselGetDeviceList(&device_list);
    printf("device_list.num_devices : u8 = %u\n", device_list.num_devices);
    if (device_list.num_devices == 0) {
        printf("sensel: no device found\n");
        return 1;
    }
    if (opt.print_devices) {
        for (int i = 0 ; i < device_list.num_devices ; i++) {
            sensel_print_device_info(&device_list, i);
        }
    }
    if (opt.text_mode) {
        sensel_print_contacts();
        //sensel_print_total_force();
        //sensel_print_accel_data();
    } else {
        sensel_send_osc(opt);
    }
    printf("sensel: exit 0\n");
    return 0;
}
