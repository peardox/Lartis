have_delphi_train = False
have_delphi_style = False
have_delphi_io = False
have_fmx = False

#try:
#    from fmx import (Image)
#    have_fmx = True
#except Exception as e:
#    print("Missing fmx")

try:
    import pinout

    have_delphi_io = True

    class TDelphiInputOutput:
        def __getattr__(Self, Key):
            return pinout.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            pinout.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in pinout.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

    if have_delphi_io:
        ioopts = TDelphiInputOutput();

except Exception as e:
#    print("Missing pinout")
    pass
    

try:
    import pcalibrate

    have_delphi_calibrate = True

    class TDelphiCalibrate:
        def __getattr__(Self, Key):
            return pcalibrate.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            pcalibrate.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in pcalibrate.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

except Exception as e:
#    print("Missing pcalibrate")
    pass
    
try:
    import pstyle

    have_delphi_style = True

    class TDelphiStylize:
        def __getattr__(Self, Key):
            return pstyle.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            pstyle.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in pstyle.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

except Exception as e:
#    print("Missing pstyle")
    pass
    
try:
    import ptrain

    have_delphi_train = True

    class TDelphiTrain:
        def __getattr__(Self, Key):
            return ptrain.GetProperty(Key)

        def __setattr__(Self, Key, Value):
            ptrain.SetProperty(Key, Value)

        def __repr__(Self):
            tmp = ""
            for i in ptrain.GetPropertyList():
                if tmp:
                    tmp = tmp + ", "
                tmp = tmp + i + " = " + str(getattr(Self,i))
            return tmp

except Exception as e:
#    print("Missing ptrain")
    pass
  
class TJsonLog(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self

