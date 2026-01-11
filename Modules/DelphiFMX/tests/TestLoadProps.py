"""
Comprehensive tests for LoadProps method in DelphiFMX module.

Testing can be run with:
    `python -m unittest discover -s tests -p 'TestLoadProps.py'`
or with pytest (for nicer output, though this requires pytest to be installed):
    `pytest -v TestLoadProps.py`

Tests cover:
- Valid inputs (str, bytes, PathLike objects)
- Invalid inputs (wrong types, non-existent files)
- UTF-8 path handling
- Edge cases and error conditions
- PathLike objects with unusual behavior

Cross-platform support: Windows, Linux, macOS, Android
"""

import unittest
import os
import sys
import tempfile
import shutil
import platform
from pathlib import Path

# Ensure DelphiFMX module can be found
_test_dir = os.path.dirname(os.path.abspath(__file__))
_module_dir = os.path.dirname(_test_dir)

# Detect platform and architecture for cross-platform support
_system = platform.system()
_is_64bit = sys.maxsize > 2**32

# Detect Android (check for Android-specific indicators)
_is_android = False
if hasattr(sys, 'getandroidapilevel'):
    _is_android = True
elif 'ANDROID_ROOT' in os.environ or 'ANDROID_DATA' in os.environ:
    _is_android = True
elif _system == 'Linux' and os.path.exists('/system/build.prop'):
    _is_android = True

# Determine platform-specific paths and module extensions
if _is_android:
    _platform_dir = 'Android64' if _is_64bit else 'Android'
elif _system == 'Windows':
    _platform_dir = 'Win64' if _is_64bit else 'Win32'
elif _system == 'Linux':
    _platform_dir = 'Linux64'
elif _system == 'Darwin':  # macOS
    _platform_dir = 'OSX64' if _is_64bit else 'OSX32'
else:
    raise NotImplementedError(f"Unsupported platform: {_system}")

# Try to find the module in the pyd directory
_pyd_dir = os.path.join(_module_dir, 'pyd', 'Release', _platform_dir)

# Find and add the directory with the module
import importlib
for _module_ext in importlib.machinery.EXTENSION_SUFFIXES:
    _module_file = os.path.join(_pyd_dir, f'DelphiFMX{_module_ext}')
    if os.path.exists(_module_file):
        if _pyd_dir not in sys.path:
            sys.path.insert(0, _pyd_dir)
        print(f"Module will be loaded from: {_module_file}")
        break

# Import DelphiFMX module - fail loudly if not available
try:
    from DelphiFMX import Form
except ImportError as e:
    raise ImportError(
        f"Failed to import DelphiFMX module.\n"
        f"Tried to load from: {_pyd_dir}\n"
        f"Platform: {_system}, Android: {_is_android}, Architecture: {_platform_dir}, Extension: {_module_ext}\n"
        f"Make sure DelphiFMX{_module_ext} is built and available at:\n"
        f"  {_module_file}\n"
        f"Original error: {e}"
    ) from e


class FormForTest(Form):
    """Test form class - allows for adding subcomponents at LoadProps."""
    pass


class TestLoadProps(unittest.TestCase):
    """Test suite for LoadProps method."""
    
    # Path to the reference .fmx file in tests directory
    _TEST_FMX_SOURCE = os.path.join(os.path.dirname(__file__), 'test_form.fmx')
    
    @classmethod
    def setUpClass(cls):
        """Set up test fixtures before all tests."""
        
        if not os.path.exists(cls._TEST_FMX_SOURCE):
            raise FileNotFoundError(
                f"Test .fmx file not found: {cls._TEST_FMX_SOURCE}\n"
                "This file must exist for tests to run."
            )
        
        # Create a temporary directory for test files
        cls.test_dir = tempfile.mkdtemp(prefix='p4d_test_')
        
        # Copy the reference .fmx file to test directory
        cls.valid_fmx = os.path.join(cls.test_dir, 'test_form.fmx')
        shutil.copy2(cls._TEST_FMX_SOURCE, cls.valid_fmx)
        
        # Create UTF-8 path test directory and copy .fmx file there
        utf8_dir = os.path.join(cls.test_dir, '测试_тест_🎉')
        os.makedirs(utf8_dir, exist_ok=True)
        cls.utf8_fmx = os.path.join(utf8_dir, 'form_测试.fmx')
        shutil.copy2(cls._TEST_FMX_SOURCE, cls.utf8_fmx)
    
    @classmethod
    def tearDownClass(cls):
        """Clean up test fixtures after all tests."""
        try:
            shutil.rmtree(cls.test_dir)
        except:
            pass
    
    def setUp(self):
        """Set up before each test."""
        # Create a fresh form for each test
        self.form = FormForTest(None)
    
    def tearDown(self):
        """Clean up after each test."""
        try:
            if hasattr(self, 'form') and self.form:
                self.form.Release()
        except:
            pass
    
    def _copy_fmx_to_path(self, target_path):
        """Helper to copy the test .fmx file to a specific path."""
        target_dir = os.path.dirname(target_path)
        if target_dir and not os.path.exists(target_dir):
            os.makedirs(target_dir, exist_ok=True)
        shutil.copy2(self._TEST_FMX_SOURCE, target_path)
        return target_path
    
    def _deny_read_access(self, path):
        """Deny read access to a file or directory using platform-specific methods.
        
        Returns a context manager that restores permissions on exit.
        Cross-platform: Windows uses win32security, Unix uses os.chmod().
        
        Raises:
            ImportError: If win32security is not available (Windows only)
            Exception: If setting permissions fails
        """
        is_windows = platform.system() == 'Windows'
        is_directory = os.path.isdir(path)
        
        if is_windows:
            try:
                import win32security
                import win32api
                import ntsecuritycon as con
            except ImportError as e:
                raise ImportError(
                    f"win32security module (pywin32) is required for permission testing on Windows. "
                    f"Install it with: pip install pywin32. Original error: {e}"
                ) from e
            
            class PermissionRestorer:
                def __init__(self, path):
                    self.path = path
                    self.user_sid = win32security.LookupAccountName(None, win32api.GetUserName())[0]
                
                def __enter__(self):
                    dacl = win32security.ACL()
                    dacl.AddAccessDeniedAce(win32security.ACL_REVISION, con.GENERIC_READ, self.user_sid)
                    
                    # Use SetNamedSecurityInfo with PROTECTED_DACL to disable inheritance
                    win32security.SetNamedSecurityInfo(self.path, win32security.SE_FILE_OBJECT,
                        win32security.DACL_SECURITY_INFORMATION | win32security.PROTECTED_DACL_SECURITY_INFORMATION,
                        None, None, dacl, None)
                    
                    return self
                
                def __exit__(self, exc_type, exc_val, exc_tb):
                    # Restore default permissions
                    dacl = win32security.ACL()
                    dacl.AddAccessAllowedAce(win32security.ACL_REVISION, con.GENERIC_ALL, self.user_sid)
                    win32security.SetNamedSecurityInfo(self.path, win32security.SE_FILE_OBJECT,
                        win32security.DACL_SECURITY_INFORMATION,
                        None, None, dacl, None)
                    return False  # Don't suppress exceptions
            
            return PermissionRestorer(path)
        else:
            import stat
            class PermissionRestorer:
                def __init__(self, path, is_directory):
                    self.path = path
                    self.is_directory = is_directory
                    self.original_mode = os.stat(path).st_mode
                
                def __enter__(self):
                    # Remove read and execute permissions (execute needed to access files in directory)
                    os.chmod(self.path, stat.S_IWRITE)  # Write-only
                    return self
                
                def __exit__(self, exc_type, exc_val, exc_tb):
                    os.chmod(self.path, self.original_mode)
                    return False  # Don't suppress exceptions
            
            return PermissionRestorer(path, is_directory)
    
    def _lock_file(self, file_path):
        """Lock a file exclusively using Windows file locking.
        
        Returns a context manager that unlocks the file on exit.
        Windows only - Unix file locking is advisory and not reliable for testing.
        
        Raises:
            ImportError: If msvcrt is not available
            Exception: If locking the file fails
        """
        if platform.system() != 'Windows':
            raise NotImplementedError("File locking test only available on Windows - Unix uses advisory locking which is not reliable")
        
        try:
            import msvcrt
        except ImportError as e:
            raise ImportError(
                f"msvcrt module is required for file locking on Windows. "
                f"Original error: {e}"
            ) from e
        
        class FileLocker:
            def __init__(self, path):
                self.path = path
                self.handle = None
                self.file_size = None
            
            def __enter__(self):
                self.handle = open(self.path, 'r+b')
                self.file_size = os.path.getsize(self.path)
                # Lock the file exclusively (lock entire file: 0 to file size)
                msvcrt.locking(self.handle.fileno(), msvcrt.LK_LOCK, self.file_size)
                return self
            
            def __exit__(self, exc_type, exc_val, exc_tb):
                if self.handle:
                    try:
                        msvcrt.locking(self.handle.fileno(), msvcrt.LK_UNLCK, self.file_size)
                    finally:
                        self.handle.close()
                return False  # Don't suppress exceptions
        
        return FileLocker(file_path)
    
    def _verify_basic_properties_loaded(self, form, msg_prefix=""):
        """Helper to verify that basic form properties were actually loaded from .fmx file.
        
        This ensures LoadProps didn't just return True without doing anything.
        """
        self.assertEqual(form.Caption, 'Form1', 
                        f"{msg_prefix}Caption should be 'Form1' after LoadProps")
        self.assertEqual(form.ClientWidth, 624, 
                        f"{msg_prefix}ClientWidth should be 624 after LoadProps")
        self.assertEqual(form.ClientHeight, 441, 
                        f"{msg_prefix}ClientHeight should be 441 after LoadProps")
    
    # ========== Valid Input Tests ==========
    
    def test_loadprops_with_string_path(self):
        """Test LoadProps with a regular string path."""
        result = self.form.LoadProps(self.valid_fmx)
        self.assertTrue(result, "LoadProps should return True for valid .fmx file")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_pathlib_path(self):
        """Test LoadProps with pathlib.Path object."""
        path_obj = Path(self.valid_fmx)
        result = self.form.LoadProps(path_obj)
        self.assertTrue(result, "LoadProps should return True for valid Path object")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_utf8_string_path(self):
        """Test LoadProps with UTF-8 characters in string path."""
        result = self.form.LoadProps(self.utf8_fmx)
        self.assertTrue(result, "LoadProps should return True for UTF-8 path")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_utf8_pathlib_path(self):
        """Test LoadProps with UTF-8 characters in Path object."""
        path_obj = Path(self.utf8_fmx)
        result = self.form.LoadProps(path_obj)
        self.assertTrue(result, "LoadProps should return True for UTF-8 Path object")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_absolute_path(self):
        """Test LoadProps with absolute path."""
        abs_path = os.path.abspath(self.valid_fmx)
        result = self.form.LoadProps(abs_path)
        self.assertTrue(result, "LoadProps should work with absolute path")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_relative_path(self):
        """Test LoadProps with relative path."""
        old_cwd = os.getcwd()
        try:
            os.chdir(self.test_dir)
            rel_path = os.path.basename(self.valid_fmx)
            result = self.form.LoadProps(rel_path)
            self.assertTrue(result, "LoadProps should work with relative path")
            self._verify_basic_properties_loaded(self.form)
        finally:
            os.chdir(old_cwd)


    def test_loadprops_with_path_containing_spaces(self):
        """Test LoadProps with path containing spaces."""
        space_dir = os.path.join(self.test_dir, 'path with spaces')
        space_file = os.path.join(space_dir, 'test file.fmx')
        self._copy_fmx_to_path(space_file)
        
        result = self.form.LoadProps(space_file)
        self.assertTrue(result, "LoadProps should work with path containing spaces")
        self._verify_basic_properties_loaded(self.form)
    

    # ========== Invalid Input Tests ==========
    
    def test_loadprops_with_nonexistent_file(self):
        """Test LoadProps with non-existent file path."""
        nonexistent = os.path.join(self.test_dir, 'nonexistent.fmx')
        
        with self.assertRaises(OSError) as context:
            self.form.LoadProps(nonexistent)
        self.assertIn(nonexistent, str(context.exception))
        self.assertIn('not found', str(context.exception))


    def test_loadprops_with_none(self):
        """Test LoadProps with None (should raise TypeError)."""
        with self.assertRaises(TypeError):
            self.form.LoadProps(None)


    def test_loadprops_with_empty_filename(self):
        """Test LoadProps with empty string as filename."""
        with self.assertRaises(OSError) as context:
            self.form.LoadProps('')
        self.assertIn('not found', str(context.exception))


    def test_loadprops_with_integer(self):
        """Test LoadProps with integer (wrong type)."""
        with self.assertRaises(TypeError):
            self.form.LoadProps(123)


    def test_loadprops_with_wrong_file_content(self):
        """Test LoadProps with file that exists but wrong content."""
        txt_file = os.path.join(self.test_dir, 'test_wrong_content.fmx')
        with open(txt_file, 'w', encoding='utf-8') as f:
            f.write('not a fmx file')
        
        with self.assertRaises(RuntimeError) as context:
            self.form.LoadProps(txt_file)
        self.assertIn('EParserError', str(context.exception))


    def test_loadprops_with_empty_file(self):
        """Test LoadProps with empty file."""
        empty_file = os.path.join(self.test_dir, 'empty.fmx')
        with open(empty_file, 'w', encoding='utf-8'):
            pass
        
        with self.assertRaises(RuntimeError) as context:
            self.form.LoadProps(empty_file)
        self.assertIn('EReadError', str(context.exception))
    

    # ========== PathLike Object Edge Cases ==========

    def test_loadprops_with_custom_pathlike(self):
        """Test LoadProps with custom PathLike object."""
        class CustomPathLike:
            def __init__(self, path):
                self.path = path
            def __fspath__(self):
                return self.path
        
        result = self.form.LoadProps(CustomPathLike(self.valid_fmx))
        self.assertTrue(result, "LoadProps should work with custom PathLike")
        self._verify_basic_properties_loaded(self.form)

    
    def test_loadprops_with_pathlike_raising_exception(self):
        """Test LoadProps with PathLike that raises exception in __fspath__."""
        class ExceptionPathLike:
            def __fspath__(self):
                raise ValueError("Custom exception from __fspath__")
        
        # The exception from __fspath__ should propagate
        with self.assertRaises(ValueError) as context:
            self.form.LoadProps(ExceptionPathLike())
        self.assertIn("Custom exception from __fspath__", str(context.exception))


    def test_loadprops_with_pathlike_returning_none(self):
        """Test LoadProps with PathLike that returns None from __fspath__."""
        class NonePathLike:
            def __fspath__(self):
                return None
        
        with self.assertRaises(TypeError) as context:
            self.form.LoadProps(NonePathLike())
        self.assertIn('Python function `__fspath__` should return value of following type(s): str or bytes. Instead type `NoneType` was returned.', str(context.exception))


    def test_loadprops_with_pathlike_returning_integer(self):
        """Test LoadProps with PathLike that returns integer from __fspath__."""
        class IntPathLike:
            def __fspath__(self):
                return 42
        
        with self.assertRaises(TypeError) as context:
            self.form.LoadProps(IntPathLike())
        self.assertIn('Python function `__fspath__` should return value of following type(s): str or bytes. Instead type `int` was returned.', str(context.exception))


    def test_loadprops_with_pathlike_being_not_callable(self):
        """Test LoadProps with PathLike that returns integer from __fspath__."""
        class NonCallablePathLike:
            def __init__(self, path):
                self.__fspath__ = path

        with self.assertRaises(TypeError) as context:
            self.form.LoadProps(NonCallablePathLike(self.valid_fmx))
        self.assertIn('Expected argument type(s): str, bytes or os.PathLike', str(context.exception))


    def test_loadprops_with_pathlike_utf8(self):
        """Test LoadProps with custom PathLike returning UTF-8 path."""
        class UTF8PathLike:
            def __init__(self, path):
                self.path = path
            def __fspath__(self):
                return self.path
        
        result = self.form.LoadProps(UTF8PathLike(self.utf8_fmx))
        self.assertTrue(result, "LoadProps should work with UTF-8 PathLike")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_bytes_path(self):
        """Test LoadProps with bytes object as path."""
        bytes_path = self.valid_fmx.encode('utf-8')
        result = self.form.LoadProps(bytes_path)
        self.assertTrue(result, "LoadProps should work with bytes path")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_bytes_path_utf8(self):
        """Test LoadProps with UTF-8 bytes path."""
        bytes_path = self.utf8_fmx.encode('utf-8')
        result = self.form.LoadProps(bytes_path)
        self.assertTrue(result, "LoadProps should work with UTF-8 bytes path")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_pathlike_returning_bytes(self):
        """Test LoadProps with PathLike that returns bytes from __fspath__."""
        class BytesPathLike:
            def __init__(self, path):
                self.path = path
            def __fspath__(self):
                return self.path.encode('utf-8')
        
        bytes_pathlike = BytesPathLike(self.valid_fmx)
        result = self.form.LoadProps(bytes_pathlike)
        self.assertTrue(result, "LoadProps should work with PathLike returning bytes")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_pathlike_returning_bytes_utf8(self):
        """Test LoadProps with PathLike returning UTF-8 bytes."""
        class UTF8BytesPathLike:
            """PathLike that returns UTF-8 bytes."""
            def __init__(self, path):
                self.path = path
            def __fspath__(self):
                return self.path.encode('utf-8')
        
        utf8_bytes_pathlike = UTF8BytesPathLike(self.utf8_fmx)
        result = self.form.LoadProps(utf8_bytes_pathlike)
        self.assertTrue(result, "LoadProps should work with PathLike returning UTF-8 bytes")
        self._verify_basic_properties_loaded(self.form)


    def test_loadprops_with_pathlike_returning_bytes_invalid_encoding(self):
        """Test LoadProps with PathLike returning bytes with invalid encoding."""

        class NonUTF8BytesPathLike:
            def __fspath__(self):
                return b'\xff\xfe\x00\x01'
        
        if platform.system() == 'Windows':
            with self.assertRaises(UnicodeDecodeError) as context:
                self.form.LoadProps(NonUTF8BytesPathLike())
            self.assertEqual("'utf-8' codec can't decode byte 0xff in position 0: invalid start byte", str(context.exception))
        else: # On Linux this is actually valid path, so we actually dont find the file
            with self.assertRaises(OSError) as context:
                self.form.LoadProps(NonUTF8BytesPathLike())
            self.assertIn('not found', str(context.exception))
            self.assertIn(os.fsdecode(NonUTF8BytesPathLike().__fspath__()), str(context.exception))


    def test_loadprops_overwrites_existing_properties(self):
        """Test that LoadProps overwrites existing form properties."""
        self.form.Caption = 'Initial Caption'
        self.form.ClientWidth = 100
        self.form.ClientHeight = 100
        
        result = self.form.LoadProps(self.valid_fmx)
        self.assertTrue(result)
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_file_no_read_permission(self):
        """Test LoadProps with file that has no read permissions."""
        no_read_file = os.path.join(self.test_dir, 'no_read.fmx')
        self._copy_fmx_to_path(no_read_file)
        
        with self._deny_read_access(no_read_file):
            with self.assertRaises(OSError) as context:
                self.form.LoadProps(no_read_file)
            self.assertIn('denied', str(context.exception))
            self.assertIn('EFOpenError', str(context.exception))
            self.assertIn(no_read_file, str(context.exception))

    def test_loadprops_with_directory_no_read_permission(self):
        """Test LoadProps with file in directory that has no read permissions."""
        no_read_dir = os.path.join(self.test_dir, 'no_read_dir')
        os.makedirs(no_read_dir, exist_ok=True)
        file_in_no_read_dir = os.path.join(no_read_dir, 'test.fmx')
        self._copy_fmx_to_path(file_in_no_read_dir)
        
        with self._deny_read_access(no_read_dir):
            with self.assertRaises(OSError) as context:
                self.form.LoadProps(file_in_no_read_dir)
            # Not readable directory should lead to file not found or permission error
            self.assertIn(file_in_no_read_dir, str(context.exception))
            self.assertIn('not found', str(context.exception))

    def test_loadprops_with_locked_file(self):
        """Test LoadProps with file that is locked by another process.
        
        Windows only - Unix file locking is advisory and not reliable for testing.
        """
        if platform.system() != 'Windows':
            self.skipTest("File locking test only available on Windows - Unix uses advisory locking")
        
        locked_file = os.path.join(self.test_dir, 'locked.fmx')
        self._copy_fmx_to_path(locked_file)
        
        with self._lock_file(locked_file):
            with self.assertRaises(OSError) as context:
                self.form.LoadProps(locked_file)
            self.assertIn(locked_file, str(context.exception))
            self.assertIn('EFOpenError', str(context.exception))

    def test_loadprops_with_corrupted_binary_file(self):
        """Test LoadProps with file that looks like binary but is corrupted."""
        corrupted_file = os.path.join(self.test_dir, 'corrupted.fmx')
        # Write some binary data that might look like a FMX but is corrupted
        with open(corrupted_file, 'wb') as f:
            f.write(b'TPF0')  # Valid signature
            f.write(b'a' * 100)

        with self.assertRaises(RuntimeError) as context:        
            self.form.LoadProps(corrupted_file)
        self.assertTrue(str(context.exception).startswith('EReadError'), f"Expected EReadError, got: {context.exception}")


def run_tests():
    """Run all tests."""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestLoadProps))
    
    # Run tests with default unittest runner
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Return exit code
    return 0 if result.wasSuccessful() else 1


if __name__ == '__main__':
    sys.exit(run_tests())

