program MRIcroN;

uses
  Forms,
  nifti_img_view in 'nifti_img_view.pas' {ImgForm},
  nifti_hdr_view in 'nifti_hdr_view.pas' {HdrForm},
  nifti_img in 'nifti_img.pas',
  render in 'render.pas' {RenderForm},
  ROIfilt in 'ROIfilt.pas' {FilterROIform},
  autoroi in 'autoroi.pas' {AutoROIForm},
  smoothVOI in 'smoothVOI.pas' {SmoothVOIForm},
  cutout in 'cutout.pas' {CutoutForm},
  MultiSlice in 'MultiSlice.pas' {MultiSliceForm},
  Text in 'Text.pas' {TextForm},
  ReadInt in 'ReadInt.pas' {ReadIntForm},
  nifti_hdr in 'nifti_hdr.pas',
  histoform in 'histoform.pas' {HistogramForm},
  about in 'about.pas' {AboutForm},
  cropedges in 'cropedges.pas' {CropEdgeForm},
  bet in 'bet.pas' {BETForm},
  mni in 'mni.pas' {MNIForm},
  graphx in 'graphx.pas' {Graph4DForm},
  perisettings in 'perisettings.pas' {PSForm},
  prefs in 'prefs.pas' {PrefForm},
  fill in 'fill.pas',
  readfloat in 'readfloat.pas',
  periutils in 'periutils.pas',
  imgutil in 'imgutil.pas',
  rotation in 'rotation.pas' {RotationForm},
  dilate in 'dilate.pas',
  sliceinterpolate in 'sliceinterpolate.pas',
  landmarks in 'landmarks.pas' {AnatForm},
  otsu in 'otsu.pas';

{$R *.RES}
{x$R MYRES.RES}
begin
  Application.Initialize;
  Application.Title := 'MRIcroN';
  Application.CreateForm(TImgForm, ImgForm);
  Application.CreateForm(THdrForm, HdrForm);
  Application.CreateForm(TRenderForm, RenderForm);
  Application.CreateForm(TFilterROIform, FilterROIform);
  Application.CreateForm(TAutoROIForm, AutoROIForm);
  Application.CreateForm(TSmoothVOIForm, SmoothVOIForm);
  Application.CreateForm(TCutoutForm, CutoutForm);
  Application.CreateForm(TMultiSliceForm, MultiSliceForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.CreateForm(TReadIntForm, ReadIntForm);
  Application.CreateForm(THistogramForm, HistogramForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TCropEdgeForm, CropEdgeForm);
  Application.CreateForm(TBETForm, BETForm);
  Application.CreateForm(TMNIForm, MNIForm);
  Application.CreateForm(TGraph4DForm, Graph4DForm);
  Application.CreateForm(TPSForm, PSForm);
  Application.CreateForm(TPrefForm, PrefForm);
  Application.CreateForm(TReadFloatForm, ReadFloatForm);
  Application.CreateForm(TRotationForm, RotationForm);
  Application.CreateForm(TAnatForm, AnatForm);
  Application.Run;
end.
