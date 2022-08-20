import os
import sys
try:
    from delphifuncts import *
    print("Delphi OK")
except Exception as e:
    # Most likely running from command line
    have_delphi = False
    __embedded_python__ = False
    sys.path.append('pysrc')
    from delphifuncts import *
    print("Delphi Missing")
   
import time
import logging
import torch
import json
from mlfuncts import *

have_psutils = True
try:
    import psutil
except Exception as e:
    have_psutils = False

def get_sys_info():
    if gpu_supported:
        d = torch.cuda.get_device_name(0)
        t = torch.cuda.get_device_properties(0).total_memory
        r = torch.cuda.memory_reserved(0)
        a = torch.cuda.memory_allocated(0)
        f = r-a  # free inside reserved
    
        gpu = TJsonLog(
            device = d,
            free = f,
            reserved = r,
            allocated = a,
            total = t)

    if have_psutils:
        m = psutil.virtual_memory()
        mem = TJsonLog(
            total = m.total,
            available = m.available,
            percent = m.percent,
            used = m.used,
            free = m.free)

    if have_psutils and gpu_supported:
        stats = TJsonLog(gpu = gpu, mem = mem)
    elif have_psutils and not gpu_supported:
        stats = TJsonLog(gpu = False, mem = mem)
    elif not have_psutils and gpu_supported:
        stats = TJsonLog(gpu = gpu, mem = False)
    else:
        stats = TJsonLog(gpu = False, mem = False)
        
    return(stats)
    

def check_gpu():
    gpu_supported = False
    try:
        torch.cuda.init()
        if(torch.cuda.is_available()):
            gpu_supported = True
    except:
        pass

    return gpu_supported

def show_elapsed(from_time):
    elapsed = time.time() - from_time
    print("Elapsed time = %f secs" % (elapsed))
    hour = elapsed // 3600
    elapsed %= 3600
    minutes = elapsed // 60
    elapsed %= 60
    seconds = elapsed
    print("Elapsed time = %d hours %d mins %d secs" % (hour, minutes, seconds))

def do_stylize(opts = None):
    is_gpu_available = check_gpu()
    
    if opts == None:
        opts = TStylize( content_image = "input-images/haywain.jpg",
            content_image_raw = "",
            output_image = "output-images/command-test.jpg",
            model = "dae_mosaic_1-200",
            model_dir = "models",
            model_ext = ".pth",
            logfile = "",
            content_scale = 1,
            ignore_gpu = True,
            export_onnx = False,
            add_model_ext = True,
            log_event_api = False,
            calibrating = False)

    start = time.time()
    if opts.ignore_gpu:
        stylize(opts, False)
    else:
        stylize(opts, is_gpu_available)
    show_elapsed(start)

def do_cmd_style(opts = None):
    # is_gpu_available = check_gpu()
    
    if opts == None:
        opts = TStylize( content_image = "input-images\\calibration.jpg",
            content_image_raw = "",
            output_image = "output-images\\calibration.jpg",
            model = "mosaic-100",
            model_dir = "models/mosain",
            model_ext = ".pth",
            logfile = "",
            content_scale = 1,
            ignore_gpu = False,
            export_onnx = False,
            add_model_ext = True,
            log_event_api = False,
            calibrating = False
            )

    for k, v in opts.items():
        print(k, '=', v)

def delphi_train(trainopts = None):
    is_gpu_available = check_gpu()

    if trainopts == None:
        trainopts = TDelphiTrain()
        for i in ptrain.GetPropertyList():
            print(i, '=', ptrain.GetProperty(i))
    
    rval = None
    
    trial_batch = trainopts.batch_size
    
    start = time.time()
    
    while(1):
        oom = False
        try:
            if trial_batch == 0:
                break
            if not trainopts.calibrating:
                print("Trying batch of ", trial_batch)
                
            if trainopts.ignore_gpu:
                rval = train(trainopts, False, trial_batch)
            else:
                rval = train(trainopts, is_gpu_available, trial_batch)
        except RuntimeError as e:
            if trainopts.calibrating:
                break
            else:
                print("Hit exception handler")
                if trial_batch > 0:
                    oom = True
                else:
                    print(e)
                    return("Unrecoverable Error")
        else:
            break

        if oom:
            trial_batch -= 1
            if is_gpu_available and not trainopts.ignore_gpu:
                torch.cuda.empty_cache()

    if trial_batch == 0:
        print("No batch size found to run current training session (style image too large)")

    if not trainopts.calibrating:
        show_elapsed(start)
        
    return (rval)
    
def do_cmd_calibration_train(batch):
    opts = TTrain(dataset="datasets/train/unsplash/lite/256",
        style_image="temp/calibration-input.jpg",
        model_name="calibration",
        model_dir="temp",
        checkpoint_model_dir="",
        model_ext = ".pth",
        net="vgg19",
        vgg16_path=None,
        vgg19_path=None,
        logfile="",
        epochs=1,
        limit=10,
        batch_size=batch,
        image_size=256,
        seed=42,
        content_weight=1e5,
        style_weight=1e10,
        lr=1e-3,
        style_scale=1.0,
        channels=32,
        force_size=True,
        ignore_gpu=False,
        log_event_api=False,
        calibrating = True)
        
    return delphi_train(opts)

def cmd_calibration_train(batch = 1):
    import PIL

    fail_count = 0
    starting_size = 16
    size = starting_size
    window_size = size
    calibration_result = True
    last_good = size
#    print("Batch =", batch)
    
    while calibration_result and (window_size >= 1):
#        print("Trying", size);
        if os.path.isfile('temp/calibration-input.jpg'):
            os.remove('temp/calibration-input.jpg')
        im = PIL.Image.new(mode="RGB", size=(size, size), color = (153, 153, 255))
        im.save('temp/calibration-input.jpg')
        torch.cuda.empty_cache()
        calibration_result = do_cmd_calibration_train(batch)
        if calibration_result:
#            print('Good -', size, '-', calibration_result)
            last_good = size
            if window_size == size:
                size = size * 2
                window_size = size
            else:
                window_size = window_size // 2;
                size = size + window_size
        else:
#            print('Bad -', size, '-', calibration_result)
            fail_count = fail_count + 1
            if window_size == size:
                window_size = window_size // 4;
                size = size - window_size
            else:
                window_size = window_size // 2;
                size = size - window_size
            if fail_count < 16:
                calibration_result = True
                
    if last_good == starting_size:
        return 0
    else:
        return last_good

        
def delphi_style(styleopts = None):
    is_gpu_available = check_gpu()

    if styleopts == None:
        styleopts = TDelphiStylize()

        for i in pstyle.GetPropertyList():
            print(i, '=', pstyle.GetProperty(i))

    if styleopts.ignore_gpu and is_gpu_available:
        print("Ignoring GPU")
        is_gpu_available = False
    
    rval = False
    oom = False

    retry = True
    while retry:
        retry = False
        try:
            start = time.time()
            if not is_gpu_available:
                if not styleopts.calibrating:
                    print("Styling without GPU")
                rval = stylize(styleopts, False)
            else:
                if not styleopts.calibrating:
                    print("Styling with GPU")
                rval = stylize(styleopts, is_gpu_available)
        except RuntimeError as e:
            # print("Hit exception handler, is_gpu_available =", is_gpu_available)
            if styleopts.calibrating:
                pass
            else:
                oom = True
                if is_gpu_available:
                    retry = True
                    is_gpu_available = False
                    print("Retry enabled")
                
    if not styleopts.calibrating:
        show_elapsed(start)
        
    return (rval)
    
def do_cmd_calibration_style():
    opts = TStylize( content_image = "input-images/calibration.jpg",
        content_image_raw = "",
        output_image = "output-images/calibration.jpg",
        model = "mosaic-100",
        model_dir = "models/mosaic",
        model_ext = ".pth",
        logfile = "",
        content_scale = 1,
        ignore_gpu = False,
        export_onnx = False,
        add_model_ext = True,
        log_event_api = False,
        calibrating = True
        )
        
    return delphi_style(opts)

def cmd_calibration_style():
    import PIL

    fail_count = 0
    starting_size = 16
    size = starting_size
    window_size = size
    calibration_result = True
    last_good = size
    
    while calibration_result and (window_size >= 1):
#        print("Trying", size);
        if os.path.isfile('input-images/calibration.jpg'):
            os.remove('input-images/calibration.jpg')
        if os.path.isfile('output-images/calibration.jpg'):
            os.remove('output-images/calibration.jpg')
        im = PIL.Image.new(mode="RGB", size=(size, size), color = (153, 153, 255))
        im.save('input-images/calibration.jpg')
        calibration_result = do_cmd_calibration_style()
        if calibration_result:
#            print('Good -', size, '-', calibration_result)
            last_good = size
            if window_size == size:
                size = size * 2
                window_size = size
            else:
                window_size = window_size // 2;
                size = size + window_size
        else:
#            print('Bad -', size, '-', calibration_result)
            fail_count = fail_count + 1
            if window_size == size:
                window_size = window_size // 4;
                size = size - window_size
            else:
                window_size = window_size // 2;
                size = size - window_size
            if fail_count < 16:
                calibration_result = True
                
    if last_good == starting_size:
        return 0
    else:
        return last_good

def do_main():
    print("Running from command line")
    # do_stylize()
    # print("Biggest image =", cmd_calibration_style())
    for batch in range(64):
        biggest = cmd_calibration_train(batch + 1)
        if biggest > 0:
            print(batch + 1, ",", biggest)
        else:
            break
            
class TStylize(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self
        
class TTrain(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self

class TProperties:
    def __getattr__(Self, Key):
        return props.GetProperty(Key)

    def __setattr__(Self, Key, Value):
        props.SetProperty(Key, Value)

    def __repr__(Self):
        tmp = ""
        for i in props.GetPropertyList():
            if tmp:
                tmp = tmp + ", "
            tmp = tmp + i + " = " + str(getattr(Self,i))
        return tmp

try:
    if not __embedded_python__:
        do_main()
except NameError:
    # Only run main if called explicitly
    if __name__ == "__main__":
        do_main()
else:
    pass
#    gpu_supported = check_gpu()
#    print("Using Embedded Environment")
#    print(json.dumps(get_sys_info()))

#    styleopts = TDelphiStylize()
#    for i in pstyle.GetPropertyList():
#        print(i, '=', pstyle.GetProperty(i))


#    trainopts = TDelphiTrain()
#    for i in ptrain.GetPropertyList():
#        print(i, '=', ptrain.GetProperty(i))
