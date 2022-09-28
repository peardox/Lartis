import os
import sys

# version = 1.0.1

try:
    from delphifuncts import *
    have_delphi = True
    print("Delphi OK")
except Exception as e:
    # Most likely running from command line
    have_delphi = False
    __embedded_python__ = False
    sys.path.append('pysrc')
    from delphifuncts import *

import time
import logging
import torch
import json
from mlfuncts import *
import math
from collections import namedtuple

have_psutil = False
try:
    import psutil
    have_psutil = True
except Exception as e:
    pass

def check_gpu():
    gpu_supported = False

    if sys.platform == 'darwin':
        osx_ver = os.popen("sw_vers -buildVersion").read().strip()
        if osx_ver >= '21E230' and torch.has_mps:
            gpu_supported = True
    else:
        try:
            torch.cuda.init()
            if(torch.cuda.is_available()):
                gpu_supported = True
        except:
            pass

    return gpu_supported

def show_elapsed(from_time, show_result = False):
    elapsed = time.time() - from_time
    elapsed_seconds = elapsed
    if show_result:
        print("Elapsed time = %f secs" % (elapsed))
    hour = elapsed // 3600
    elapsed %= 3600
    minutes = elapsed // 60
    elapsed %= 60
    seconds = elapsed
    if show_result:
        print("Elapsed time = %d hours %d mins %d secs" % (hour, minutes, seconds))
    return elapsed_seconds

def square_to_aspect(size, aspect):
    if aspect == 1:
        return (size, size)
    else:
        sqr = size * size 
        height = math.floor(math.sqrt(sqr * (1/aspect)))
        width = math.floor(height * aspect)
        return (width, height)
        
    
def do_stylize(opts = None):
    is_gpu_available = check_gpu()
    
    if opts is None:
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
    
    if opts is None:
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

    if trainopts is None:
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
                print(e)
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
            if is_gpu_available and not trainopts.ignore_gpu and not torch.has_mps:
                torch.cuda.empty_cache()

    if trial_batch == 0:
        print("No batch size found to run current training session (style image too large)")

    if not trainopts.calibrating:
        show_elapsed(start)
        
    return (rval)
    
def do_cmd_calibration_train(batch, ignore_gpu):
    opts = TTrain(dataset="datasets/train/unsplash/lite/256",
        style_image="temp/train-calibration-input.jpg",
        model_name="train-calibration",
        model_dir="temp",
        checkpoint_model_dir="",
        model_ext = ".pth",
        net="vgg19",
        vgg16_path=None,
        vgg19_path='pretrained/vgg19-dcbb9e9d.pth',
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
        ignore_gpu=ignore_gpu,
        log_event_api=False,
        calibrating = True)
        
    return delphi_train(opts)

def cmd_calibration_train(batch = 1, aspect = None, ignore_gpu = False, one_shot = 0):
    import PIL

    debug = False
    fail_count = 0    
    if aspect is None:
        aspect = 1

    starting_size = 32
    size = starting_size
    window_size = size
    calibration_result = True
    last_good = size
    
    while calibration_result and (window_size >= 16):
        error_flag = False
        if os.path.isfile('temp/train-calibration-input.jpg'):
            os.remove('temp/train-calibration-input.jpg')
        if one_shot > 0:
            width = one_shot
            height = one_shot
        else:
            width, height  = square_to_aspect(size, aspect)
        if debug:
            print(width, height)
        im = PIL.Image.new(mode="RGB", size=(width, height), color = (153, 153, 255))
        im.save('temp/train-calibration-input.jpg')
        if not torch.has_mps:
            torch.cuda.empty_cache()
        try:
            start_style = time.time()
            calibration_result = do_cmd_calibration_train(batch, ignore_gpu)
        except Exception as e:
            if e.__class__.__name__ == 'DecompressionBombError':
                print('Size Exception', last_good,  ',', width, ',' , height, ',', window_size, style_time)
            else:
                print(e)
                print('Exception type is:', e.__class__.__name__)
            calibration_result = False
            error_flag = True

        if calibration_result:
            style_time = show_elapsed(start_style, False)
            last_size = size
            if debug:
                print(1,  ',', width, ',' , height, ',', size, style_time, window_size, error_flag)
            last_good = size
            if torch.has_mps and last_good == 3072:
                break # Stupid Mac!
            if window_size == size:
                size = size * 2
                window_size = size
            else:
                window_size = window_size // 2
                size = size + window_size
            if torch.has_mps:
                if window_size == 4096:
                    window_size = 1024
                    size = 3072
            if have_delphi_io:
                # print(1,  ',', width, ',' , height, ',', size, ',', style_time)
                ioopts.CalibrateJsonLog = json.dumps(TJsonLog(result = 1, width = width, height = height, size = last_size, time = style_time, next_size = size, window_size = window_size, error_flag = error_flag, batch = 1, log = 1, done = 0))
                pcalibrate.CalibrateProgress()

        else:
            style_time = show_elapsed(start_style, False)
            last_size = size
            if debug:
                print(0,  ',', width, ',' , height, ',', size, style_time, window_size, error_flag)
            fail_count = fail_count + 1
            if window_size == size:
                window_size = window_size // 4
                size = size - window_size
            else:
                window_size = window_size // 2
                size = size - window_size
            if fail_count < 16:
                calibration_result = True
            if have_delphi_io:
                # print(0,  ',', width, ',' , height, ',', size, ',', style_time)
                ioopts.CalibrateJsonLog = json.dumps(TJsonLog(result = 0, width = width, height = height, size = last_size, time = style_time, next_size = size, window_size = window_size, error_flag = error_flag, batch = 1, log = 1, done = 0))
                pcalibrate.CalibrateProgress()
        if one_shot > 0:
            break

    if (last_good == starting_size) and (one_shot == 0):
        return 0
    else:
        if one_shot > 0:
            return math.floor(math.sqrt(width * height))
        else:
            return last_good

        
def delphi_style(styleopts = None):
    is_gpu_available = check_gpu()

    if styleopts is None:
        styleopts = TDelphiStylize()

        for i in pstyle.GetPropertyList():
            print(i, '=', pstyle.GetProperty(i))

    if styleopts.ignore_gpu and is_gpu_available:
#        print("Ignoring GPU")
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
                    # retry = True
                    # print("RetryCPU")
                    is_gpu_available = False
                    ioopts.StyleErrorLog = 'ERR_GPU_OOM'
                    pstyle.StyleError()
                    return (rval)
                    
    if not styleopts.calibrating:
        show_elapsed(start)
        if rval == False:
            ioopts.StyleErrorLog = 'ERR_OTHER'
            pstyle.StyleError()

    return (rval)

def delphi_calibration_style():
    calibration_time = time.time()
    AspectRatio = pcalibrate.GetProperty('AspectRatio')
    ignore_gpu = pcalibrate.GetProperty('ignore_gpu')
    one_shot = pcalibrate.GetProperty('OneShot')
    res = cmd_calibration_style(AspectRatio, ignore_gpu, one_shot)
    calibration_time = time.time() - calibration_time
    ioopts.CalibrationResultJson = json.dumps(TJsonLog(result = res, time = calibration_time, batch = 1, log = 0, done = 1))
    pcalibrate.CalibrateFinished()
    
    
def delphi_calibration_train():
    calibration_time = time.time()
    AspectRatio = pcalibrate.GetProperty('AspectRatio')
    ignore_gpu = pcalibrate.GetProperty('ignore_gpu')
    BatchSize = pcalibrate.GetProperty('BatchSize')
    one_shot = pcalibrate.GetProperty('OneShot')
    res = cmd_calibration_train(BatchSize, AspectRatio, ignore_gpu, one_shot)
    calibration_time = time.time() - calibration_time
    ioopts.CalibrationResultJson = json.dumps(TJsonLog(result = res, time = calibration_time, batch = BatchSize, log = 1, done = 1))
    pcalibrate.CalibrateFinished()

    
def do_cmd_calibration_style(ignore_gpu):
    opts = TStylize( content_image = "temp/style-calibration-input.jpg",
        content_image_raw = "",
        output_image = "temp/style-calibration-output.jpg",
        model = "mosaic-100",
        model_dir = "models/mosaic",
        model_ext = ".pth",
        logfile = "",
        content_scale = 1,
        ignore_gpu = ignore_gpu,
        export_onnx = False,
        add_model_ext = True,
        log_event_api = False,
        calibrating = True
        )
        
    return delphi_style(opts)

def cmd_calibration_style(aspect = None, ignore_gpu = True, one_shot = 0):
    import PIL

    debug = False
    if aspect is None:
        aspect = 1
    fail_count = 0
    starting_size = 32
    size = starting_size
    window_size = size
    calibration_result = True
    last_good = size
    rss = 0
    start_rss = 0
    PIL.Image.MAX_IMAGE_PIXELS = 16384 * 16384
    
    while calibration_result and (window_size >= 16):
        error_flag = False
        if os.path.isfile('temp/style-calibration-input.jpg'):
            os.remove('temp/style-calibration-input.jpg')
        if os.path.isfile('temp/style-calibration-output.jpg'):
            os.remove('temp/style-calibration-output.jpg')
        if one_shot > 0:
            width = one_shot
            height = one_shot
        else:
            width, height  = square_to_aspect(size, aspect)
        if debug:
            print(width, height)
        im = PIL.Image.new(mode="RGB", size=(width, height), color = (153, 153, 255))
        im.save('temp/style-calibration-input.jpg')
        try:
            start_style = time.time()
            calibration_result = do_cmd_calibration_style(ignore_gpu)
        except Exception as e:
            if e.__class__.__name__ == 'DecompressionBombError':
                print('Size Exception', last_good,  ',', width, ',' , height, ',', window_size, style_time)
            else:
                print(e)
                print('Exception type is:', e.__class__.__name__)
            calibration_result = False
            error_flag = True
        
        if have_psutil:
            rss = psutil.Process().memory_info().rss

        if start_rss == 0:
            start_rss = rss
#            print('Start rss =', start_rss)
        if calibration_result:
            style_time = show_elapsed(start_style, False)
# sbdbg
            if debug:
                print(1,  ',', width, ',' , height, ',', size, style_time, window_size, error_flag)
            last_good = size
            print('last_good =', last_good)
            last_size = size
            if (window_size == size):
                size = size * 2
                window_size = size
            else:
                window_size = window_size // 2
                size = size + window_size
            if have_delphi_io:
                # print(1,  ',', width, ',' , height, ',', size, ',', style_time)
                ioopts.CalibrateJsonLog = json.dumps(TJsonLog(result = 1, width = width, height = height, size = last_size, time = style_time, next_size = size, window_size = window_size, error_flag = error_flag, batch = 1, log = 0, done = 0))
                pcalibrate.CalibrateProgress()
        else:
            style_time = show_elapsed(start_style, False)
# sbdbg
            if debug:
                print(0,  ',', width, ',' , height, ',', size, style_time, window_size, error_flag)
            last_size = size
            print('last_good =', last_good)
            fail_count = fail_count + 1
            if window_size == size:
                window_size = window_size // 4
                size = size - window_size
            else:
                window_size = window_size // 2
                size = size - window_size
            if fail_count < 16:
                calibration_result = True
            if have_delphi_io:
                # print(0,  ',', width, ',' , height, ',', size, ',', style_time)
                ioopts.CalibrateJsonLog = json.dumps(TJsonLog(result = 0, width = width, height = height, size = last_size, time = style_time, next_size = size, window_size = window_size, error_flag = error_flag, batch = 1, log = 0, done = 0))
                pcalibrate.CalibrateProgress()
        if one_shot > 0:
            break
        
    print('final last_good =', last_good, 'Oneshot =', one_shot)
    if (last_good == starting_size) and (one_shot == 0):
        return 0
    else:
        if one_shot > 0:
            return math.floor(math.sqrt(width * height))
        else:
            return last_good

def do_main():
    argc = len(sys.argv)
    print("Running from command line")
    # do_stylize()
    if not os.path.exists('temp'):
        os.makedirs('temp')
    start_main = time.time()
    if argc > 1:
        ignore_gpu = False
        aspect = (16 / 9)
        if sys.argv[1] == 'cstyle':
            size = cmd_calibration_style(aspect, ignore_gpu)
#            size = cmd_calibration_style(aspect, ignore_gpu, (1920, 1080))
#            size = cmd_calibration_style(aspect, ignore_gpu, (3840, 2160))
#            size = cmd_calibration_style(aspect, ignore_gpu, (5760, 3240))
#            size = cmd_calibration_style(aspect, ignore_gpu, (7680, 4320))
            print("Biggest image =", size)
            return False
        if sys.argv[1] == 'ctrain':
            for batch in range(64):
#                print('Batching', batch + 1)
                biggest = cmd_calibration_train(batch + 1, aspect, ignore_gpu)
#                biggest = cmd_calibration_train(batch + 1, aspect, ignore_gpu, (1920, 1080))
#                biggest = cmd_calibration_train(batch + 1, aspect, ignore_gpu, (3840, 2160))
                if biggest > 0:
                    print(batch + 1, ",", biggest)
                else:
                    break
        show_elapsed(start_main, True)
            
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
    else:
        print('Hello World')
except NameError:
    if __name__ == "__main__":
        do_main()
else:
    pass
