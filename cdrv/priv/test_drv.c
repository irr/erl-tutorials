#include <stdio.h>
#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
} test_data;

static ErlDrvData test_drv_start(ErlDrvPort port, char *buff)
{
    test_data* d = (test_data*)driver_alloc(sizeof(test_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void test_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void test_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
    test_data* d = (test_data*)handle;
    char fn = buff[0], arg = buff[1], res;
    if (fn == 1) {
        res = f(arg);
    }
    driver_output(d->port, &res, 1);
}

ErlDrvEntry test_driver_entry = {
    NULL,               /* F_PTR init, N/A */
    test_drv_start,     /* L_PTR start, called when port is opened */
    test_drv_stop,      /* F_PTR stop, called when port is closed */
    test_drv_output,    /* F_PTR output, called when erlang has sent */
    NULL,               /* F_PTR ready_input, called when input descriptor ready */
    NULL,               /* F_PTR ready_output, called when output descriptor ready */
    "test_drv",         /* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* F_PTR control, port_command callback */
    NULL,               /* F_PTR timeout, reserved */
    NULL                /* F_PTR outputv, reserved */
};

DRIVER_INIT(test_drv) /* must match name in driver_entry */
{
    return &test_driver_entry;
}
