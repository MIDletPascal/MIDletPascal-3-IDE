(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Consts;

interface

const
// MP 3 ------------------------------------------------------------------------
  PROJECT_NAME = 'MIDletPascal';
  PROJECT_MAJOR_VERSION = '3';
  PROJECT_MINOR_VERSION = '3';
  PROJECT_LABEL_VERSION = 'CANDIDATE';
  PROJECT_VERSION =
    PROJECT_MAJOR_VERSION+'.'+PROJECT_MINOR_VERSION+' '+PROJECT_LABEL_VERSION;
  RELEASE_DATE_DAY = '09';
  RELEASE_DATE_MONTH = '04';
  RELEASE_DATE_YEAR = '2011';

  PROJECT_SITE_URL =
    'http://sourceforge.net/projects/midletpascal/';
  PROJECT_DOWNLOAD_URL =
    'http://downloads.sourceforge.net/project/midletpascal';
  PROJECT_ALL_DOWNLOADS_RSS_URL =
    'http://sourceforge.net/api/file/index/project-id/279759/mtime/desc/rss';

  RECENTS_FILENAME = 'recents.ini';

  STYLES_DIR ='Styles';  
  STUBS_DIR = 'Stubs';
  LIBS_DIR = 'Libs';
  HELP_DIR = 'Help';
  HELP_FILE = HELP_DIR+'\%s\index.htm';

  BUILD_SWITCH = '-build';

  DEFAULT_SKIN = 'Office 2007 Blue';
  DEFAULT_LANGUAGE_CODE = 'en';

  CODE_EDITOR_STYLE_MIDLET = 'Midlet';
  CODE_EDITOR_STYLE_MIDLET_DIFF_ADD = $FFAAAA; // PaleBlue
  CODE_EDITOR_STYLE_MIDLET_DIFF_DEL = $AAAAFF; // PaleRed
  CODE_EDITOR_STYLE_MIDLET_DIFF_MOD = $AAFFAA; // PaleGreen

  CODE_EDITOR_STYLE_CLASSIC = 'Classic';
  CODE_EDITOR_STYLE_CLASSIC_DIFF_ADD = $FF0000; // Blue
  CODE_EDITOR_STYLE_CLASSIC_DIFF_DEL = $0000FF; // Red
  CODE_EDITOR_STYLE_CLASSIC_DIFF_MOD = $AA00AA; // Purple

  EXTERNAL_LIBRARY_PREFIX = 'Lib_';

  EXTENSION_INI = '.ini';
  EXTENSION_BAT = '.bat';
  EXTENSION_PNG = '.png';

  EXTENSION_CES = '.ces';

  PORTABLE_FILENAME = 'portable';

  COMPILER_INTERNAL_ERROR_RETRIES = 3;

  BACKUPS_DISABLED = 0;
  BACKUPS_UNLIMITED = -1;

  EMULATORS_MAX = 16;

  CONFIG_TRUE = '-1';
  CONFIG_FALSE = '0';
  CONFIG_LEFTDOCK = 'LeftDock';
  CONFIG_RIGHTDOCK = 'RightDock';
  CONFIG_TOPDOCK = 'TopDock';
  CONFIG_BOTTOMDOCK = 'BottomDock';
  CONFIG_IDE_SECTION = 'IDE';
  CONFIG_IDE_SECTION_SKIN = 'Skin';
  CONFIG_IDE_SECTION_WELCOMEPAGE = 'WelcomePage';
  CONFIG_IDE_SECTION_WELCOMEPAGE_DEFAULT = CONFIG_TRUE;
  CONFIG_IDE_SECTION_GROUPMANAGER = 'GroupManager';
  CONFIG_IDE_SECTION_GROUPMANAGER_DEFAULT = CONFIG_TRUE;
  CONFIG_IDE_SECTION_PROJECTMANAGER = 'ProjectManager';
  CONFIG_IDE_SECTION_PROJECTMANAGER_DEFAULT = CONFIG_TRUE;
  CONFIG_IDE_SECTION_GROUPMANAGER_POSITION = 'GroupManagerPosition';
  CONFIG_IDE_SECTION_GROUPMANAGER_POSITION_DEFAULT = CONFIG_LEFTDOCK;
  CONFIG_IDE_SECTION_PROJECTMANAGER_POSITION = 'ProjectManagerPosition';
  CONFIG_IDE_SECTION_PROJECTMANAGER_POSITION_DEFAULT = CONFIG_LEFTDOCK;
  CONFIG_IDE_SECTION_FULLSCREEN = 'FullScreen';
  CONFIG_IDE_SECTION_FULLSCREEN_DEFAULT = CONFIG_FALSE;
  CONFIG_IDE_SECTION_MINIMIZETOTRAY = 'MinimizeToTray';
  CONFIG_IDE_SECTION_MINIMIZETOTRAY_DEFAULT = CONFIG_FALSE;
  CONFIG_IDE_SECTION_MAXBACKUPS = 'MaxBackups';
  CONFIG_IDE_SECTION_MAXBACKUPS_DEFAULT = BACKUPS_UNLIMITED;
  CONFIG_IDE_SECTION_LANGUAGE = 'Language';
  CONFIG_IDE_SECTION_LANGUAGE_DEFAULT = 'en';
  CONFIG_IDE_SECTION_CODEEDITORSTYLE = 'CodeEditorStyle';
  CONFIG_IDE_SECTION_CODEEDITORSTYLE_DEFAULT = CODE_EDITOR_STYLE_MIDLET;
  CONFIG_IDE_SECTION_CODEEDITORFONTNAME = 'CodeEditorFontName';
  CONFIG_IDE_SECTION_CODEEDITORFONTNAME_DEFAULT = 'Courier New';
  CONFIG_IDE_SECTION_CODEEDITORFONTSIZE = 'CodeEditorFontSize';
  CONFIG_IDE_SECTION_CODEEDITORFONTSIZE_DEFAULT = 10;
  CONFIG_EMULATOR_SECTION = 'EMULATOR';
  CONFIG_EMULATOR_SECTION_EMULATORS = 'Emulators';
  CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME = 'CurrentEmulatorName';
  CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME_DEFAULT = 'Default';
  CONFIG_EMULATOR_SECTION_COMMANDLINE_MP2_DEFAULT = 'cmd /A /C %JAD%';
  CONFIG_IDE_SECTION_LEFTDOCK_WIDTH = 'LeftDockWidth';
  CONFIG_IDE_SECTION_LEFTDOCK_WIDTH_DEFAULT = '160';
  CONFIG_IDE_SECTION_RIGHTDOCK_WIDTH = 'RightDockWidth';
  CONFIG_IDE_SECTION_RIGHTDOCK_WIDTH_DEFAULT = '160';
  CONFIG_OPTIONS_SECTION = 'IDE';
  CONFIG_OPTIONS_SECTION_DEFAULTPROJECTLOCATION = 'DefaultProjectLocation';

  PROJECT_MANAGER_PROPERTIES_PANEL_HEIGHT_DEFAULT = 200;
  PROJECT_MANAGER_WIDTH_DEFAULT = 160;

  GROUP_MANAGER_WIDTH_DEFAULT = 160;
  GROUP_MANAGER_HEIGHT_DEFAULT = 160;

  DIR_HISTORY = 'history';
  DIR_SCRIPTS = 'scripts';
  SCRIPT_AFTER_SUCCESSFUL_BUILD = 'afterSuccessfulBuild';
  SCRIPT_AFTER_UNSUCCESSFUL_BUILD = 'afterUnsuccessfulBuild';
  SCRIPT_BEFORE_RUN = 'beforeRun';

  COMPILER_EXE = 'mp3CC.exe';

  PREPROCESSOR_DLL = 'mp3PP.dll';
  PREPROCESSOR_PREPROCESS_FUNCTION = 'preprocess';

  COMMA = ';';

  DEFINE_J2ME = 'J2ME';
  DEFINE_MPC = 'MPC';
  DEFINE_MP30 = 'MIDLETPASCAL30';
  DEFINE_MP31 = 'MIDLETPASCAL31';
  DEFINE_MP32 = 'MIDLETPASCAL32';
  DEFINE_MP33 = 'MIDLETPASCAL33';

  MPDEFINES =
    DEFINE_J2ME + COMMA +
    DEFINE_MPC + COMMA +
    DEFINE_MP30 + COMMA +
    DEFINE_MP31 + COMMA +
    DEFINE_MP32 + COMMA +
    DEFINE_MP33;

  XML_ENCODING_TAG = '<?xml version="1.0" encoding="UTF-8" ?>';

  UNIT_NAME_LENGTH_MINIMUM = 3;

  NEW_UNIT_PATTERN =
    'unit %s;'+#10+
    #10+
    'interface'+#10+
    #10+
    'implementation'+#10+
    #10+
    'end.';

  DEFAULT_AUTO_COMPLETE_LIST =
    '[arrayd | Array-Declaration (var)]'+#10+
    'array[0..|] of ;'+#10+
    #10+
    '[arrayc | Array-Declaration (const)]'+#10+
    'array[0..|] of = ();'+#10+
    #10+
    '[fors | for (without begin/end)]'+#10+
    'for | :=  to  do'+#10+
    #10+
    '[forb | for (with begin/end)]'+#10+
    'for | :=  to  do'+#10+
    'begin'+#10+
    #10+
    'end;'+#10+
    #10+
    '[function | function declaration]'+#10+
    'function |(): ;'+#10+
    'begin'+#10+
    #10+
    'end;'+#10+
    #10+
    '[ifs | if (without begin/end)]'+#10+
    'if | then'+#10+
    #10+
    '[ifb | if (with begin/end)]'+#10+
    'if | then'+#10+
    'begin'+#10+
    #10+
    'end;'+#10+
    #10+
    '[ife | if then (without begin/end) else (without begin/end)]'+#10+
    'if | then'+#10+
    #10+
    'else'+#10+
    #10+
    '[ifeb |  if then else]'+#10+
    'if | then'+#10+
    'begin'+#10+
    #10+
    'end'+#10+
    'else'+#10+
    'begin'+#10+
    #10+
    'end;'+#10+
    #10+
    '[procedure | procedure declaration]'+#10+
    'procedure |();'+#10+
    'begin'+#10+
    #10+
    'end;'+#10+
    #10+
    '[whileb | while (with begin/end)]'+#10+
    'while | do'+#10+
    'begin'+#10+
    #10+
    'end;'+#10+
    #10+
    '[whiles | while (without begin/end)]'+#10+
    'while | do';

  PERMISSION_HTTP = 'javax.microedition.io.Connector.http';
  PERMISSION_SMS = 'javax.wireless.messaging.sms.send';

  MANIFEST_MF = 'MANIFEST.MF';
  MANIFEST_ARCHIVE_DIR = 'META-INF';

  MANIFEST_TEMPLATE =
    'Manifest-Version: 1.0'#10+
    'MIDlet-1: %PROJECT_NAME%, %PROJECT_ICON%, FW'#10+
    'MIDlet-Name: %PROJECT_NAME%'#10+
    'MIDlet-Vendor: %PROJECT_VENDOR%'#10+
    'MIDlet-Version: %PROJECT_VERSION%'#10+
    'MicroEdition-Configuration: %CLDC_VERSION%'#10+
    'MicroEdition-Profile: %MIDP_VERSION%'#10+
    'MIDlet-Icon: %PROJECT_ICON%'#10+
    'Created-By: 1.1.0_00 (MIDletPascal)';

  PATTERN_PROJECT_NAME = '%PROJECT_NAME%';
  PATTERN_PROJECT_ICON = '%PROJECT_ICON%';
  PATTERN_PROJECT_VENDOR = '%PROJECT_VENDOR%';
  PATTERN_PROJECT_VERSION = '%PROJECT_VERSION%';
  PATTERN_CLDC_VERSION = '%CLDC_VERSION%';
  PATTERN_MIDP_VERSION = '%MIDP_VERSION%';

  PATTERN_JAR_URL = '%JAR_URL%';
  PATTERN_JAR_SIZE = '%JAR_SIZE%';
  PATTERN_JAR = '%JAR%';
  PATTERN_JAD = '%JAD%';

  JAD_TEMPLATE =
    'MIDlet-1: %PROJECT_NAME%, %PROJECT_ICON%, FW'#10+
    'MIDlet-Jar-Size: %JAR_SIZE%'#10+
    'MIDlet-Jar-URL: %JAR_URL%'#10+
    'MIDlet-Name: %PROJECT_NAME%'#10+
    'MIDlet-Vendor: %PROJECT_VENDOR%'#10+
    'MIDlet-Icon: %PROJECT_ICON%'#10+
    'MIDlet-Version: %PROJECT_VERSION%'#10+
    'MicroEdition-Configuration: %CLDC_VERSION%'#10+
    'MicroEdition-Profile: %MIDP_VERSION%';

  EXTENSION_JAR = '.jar';
  EXTENSION_JAD = '.jad';
  EXTENSION_CLASS = '.class';
  EXTENSION_GROUP = '.mpgroup';
  EXTENSION_IMAGE = '.png';
  EXTENSION_PREPROCESSED = '.mppp';

  EXTENSION_PAS = '.pas';
  EXTENSION_PP = '.pp';
  EXTENSION_P = '.p';
  EXTENSION_INC = '.inc';
  EXTENSION_DPR = '.dpr';
  EXTENSION_LPR = '.lpr';

  DEFAULT_GROUP_NAME = 'NewGroup';

  DEFAULT_IMAGE_NAME = 'NewImage';
  DEFAULT_IMAGE_WIDTH = 12;
  DEFAULT_IMAGE_HEIGHT = 12;

  DEFAULT_VENDOR = 'MIDletPascal3';

// MP 1 ------------------------------------------------------------------------
  MP1_PROJECT_TAG = '### MIDlet Pascal Generated file. Do not change! ###';

// MP 2 ------------------------------------------------------------------------
// New Project Constants
  DEFAULT_PROJECT_NAME = 'NewProject';
  DEFAULT_PROJECT_LOCATION = 'C:\'+DEFAULT_PROJECT_NAME;
  DEFAULT_CONFIGURATION_NAME = 'Default';
  //DEFAULT_VENDOR = 'MIDletPascal';
  DEFAULT_VERSION = '1.0.0';
  DEFAULT_ICON_NAME = 'icon.png';
  DEFAULT_ICON = '/'+DEFAULT_ICON_NAME;
  CONFIGS_NONE = ':';
  CONFIGS_ALL = ':all:';
  DEFAULT_ICON_CONFIGS = CONFIGS_ALL;
  DEFAULT_ICON_CONTENT: array [0..283] of byte =
  (
137,  80,  78,  71,  13,  10,  26,  10,   0,   0,   0,  13,  73,  72,  68,  82,
  0,   0,   0,  12,   0,   0,   0,  12,   8,   3,   0,   0,   0,  97, 171, 172,
213,   0,   0,   0,  43, 116,  69,  88, 116,  67, 114, 101,  97, 116, 105, 111,
110,  32,  84, 105, 109, 101,   0, 110, 101, 100,  32,  55,  32, 115, 116, 117,
 32,  50,  48,  48,  52,  32,  48,  48,  58,  53,  52,  58,  53,  56,  32,  43,
 48,  49,  48,  48,  79, 151, 133, 234,   0,   0,   0,   7, 116,  73,  77,  69,
  7, 212,  11,   7,   0,   0,  17, 197, 204, 221, 195,   0,   0,   0,   9, 112,
 72,  89, 115,   0,   0,  46,  35,   0,   0,  46,  35,   1, 120, 165,  63, 118,
  0,   0,   0,   4, 103,  65,  77,  65,   0,   0, 177, 143,  11, 252,  97,   5,
  0,   0,   0,  27,  80,  76,  84,  69, 255, 255, 255, 104,   0,   0, 192,   0,
  0,  68,   2, 130, 186, 149,   2, 132,   3, 253, 254, 210,   2,   1,  47, 141,
  1,  85, 255,   0, 233, 157,  54,   0,   0,   0,   1, 116,  82,  78,  83,   0,
 64, 230, 216, 102,   0,   0,   0,  64,  73,  68,  65,  84, 120, 218, 101, 206,
 81,  10,   0,  32,   8,  68,  65,  53, 221, 188, 255, 137, 115,  37,  48, 168,
175, 121,  68, 154, 200, 119,  50,  31,  79,   5, 157,  27, 237, 170,  54, 218,
177,  54,  13,  47,  86, 152,   1,  12,  94, 152, 176, 156, 143, 218, 166, 126,
231, 209, 179,  72, 245, 255, 200,   1, 107,  83,   1,  57, 224,  85,  27,  61,
  0,   0,   0,   0,  73,  69,  78,  68, 174,  66,  96, 130
    );

  DEFAULT_PROGRAM_CODE =
    'program %s;'#10+
    'begin'#10+
    #10+
    'end.';

  DEFAULT_PROGRAM_CLASS = 'M.class';

  CLDC_10 = 'CLDC-1.0';

// MIDP Versions
  MIDP_10 = 'MIDP-1.0';
  MIDP_20 = 'MIDP-2.0';

// File Extensions
  EXTENSION_PROJECT = '.mpproj';
  EXTENSION_SOURCEFILE = '.mpsrc';

// Stubs
  FRAMEWORK_CLASS = 'FW.class';
  FORMS_CLASS = 'FS.class';
  SUPPORT_CLASS = 'S.class';
  FLOAT_CLASS = 'F.class';
  REAL_CLASS = 'Real.class';
  REALNUMBERFORMAT_CLASS = 'Real$NumberFormat.class';
  RECORDSTORE_CLASS = 'RS.class';
  HTTP_CLASS = 'H.class';
  PLAYER_CLASS = 'P.class';
  SMS_CLASS = 'SM.class';

// Project Directories
  DIR_OUTPUT = 'bin';
  DIR_CLASSES = 'classes';
  DIR_RESOURCES = 'res';
  DIR_SOURCES = 'src';

// Project XML File Tags
  TAG_PROJECT = 'Project';
  TAG_MIDLET = 'MIDlet';
    TAG_MIDLET_NAME = 'Name';
    TAG_MIDLET_VENDOR = 'Vendor';
    TAG_MIDLET_VERSION = 'Version';
    TAG_MIDLET_ICON = 'Icon';
  TAG_SOURCES = 'Sources';
    TAG_SOURCES_SOURCE = 'source';
    TAG_SOURCES_FILENAME = 'filename';
  TAG_RESOURCES = 'Resources';
    TAG_RESOURCES_RESOURCE = 'resource';
    TAG_RESOURCES_FILENAME = 'filename';
    TAG_RESOURCES_CONFIGURATION = 'configuration';
    TAG_RESOURCES_CONFIGURATIONS = 'configurations';
  TAG_BUILDCONFIGS = 'BuildConfigurations';
    TAG_BUILDCONFIGS_CONFIGURATION = 'configuration';
      TAG_BUILDCONFIGS_CONFIGURATION_NAME = 'name';
      TAG_BUILDCONFIGS_CONFIGURATION_TYPE = 'type';
      TAG_BUILDCONFIGS_CONFIGURATION_VERSION = 'version';
      TAG_BUILDCONFIGS_CONFIGURATION_MATH = 'math';
    TAG_BUILDCONFIGS_ACTIVECONFIGURATION = 'ActiveConfiguration';
      TAG_BUILDCONFIGS_ACTIVECONFIGURATION_INDEX = 'index';

// MP 3 ------------------------------------------------------------------------

  TAG_GROUP = 'Group';
  TAG_PROJECTS = 'Projects';
  TAG_PROJECTS_PROJECT = 'Project';
  TAG_PROJECTS_PROJECT_FILENAME = 'Filename';

  FILTER_ALLMPFILES =
    '(*'+EXTENSION_GROUP+';*'+EXTENSION_PROJECT+';*'+EXTENSION_SOURCEFILE+')|*'+
    EXTENSION_GROUP+';*'+EXTENSION_PROJECT+';*'+EXTENSION_SOURCEFILE;
  FILTER_PROJECTFILES = '(*'+EXTENSION_PROJECT+')|*'+EXTENSION_PROJECT;
  FILTER_SOURCEFILES = '(*'+EXTENSION_SOURCEFILE+')|*'+EXTENSION_SOURCEFILE;
  FILTER_GROUPFILES = '(*'+EXTENSION_GROUP+')|*'+EXTENSION_GROUP;
  FILTER_ALLFILES = '(*.*)|*.*';

implementation

end.
