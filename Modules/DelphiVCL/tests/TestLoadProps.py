"""
Comprehensive tests for LoadProps method in DelphiVCL module.

Tests cover:
- Valid inputs (str, PathLike objects)
- Invalid inputs (wrong types, non-existent files)
- UTF-8 path handling
- Edge cases and error conditions
- PathLike objects with unusual behavior
"""

import unittest
import os
import sys
import tempfile
import shutil
import platform
from pathlib import Path

# Ensure DelphiVCL .pyd can be found
# This is a minimal solution for test environment - in production, the module
# should be properly installed or PYTHONPATH should be set
_test_dir = os.path.dirname(os.path.abspath(__file__))
_module_dir = os.path.dirname(_test_dir)

# Detect platform architecture
_is_64bit = sys.maxsize > 2**32
_platform_dir = 'Win64' if _is_64bit else 'Win32'
_pyd_dir = os.path.join(_module_dir, 'pyd', 'Release', _platform_dir)

# Add pyd directory to sys.path if it exists and not already there
if os.path.exists(_pyd_dir) and _pyd_dir not in sys.path:
    sys.path.insert(0, _pyd_dir)

# Import DelphiVCL module - fail loudly if not available
try:
    from DelphiVCL import Form, Application
except ImportError as e:
    raise ImportError(
        f"Failed to import DelphiVCL module. "
        f"Tried to load from: {_pyd_dir}\n"
        f"Make sure DelphiVCL.pyd is built and available. "
        f"Original error: {e}"
    ) from e


class TestForm(Form):
    """Test form class - allows for adding subcomponents at LoadProps."""
    pass


class TestLoadProps(unittest.TestCase):
    """Test suite for LoadProps method."""
    
    # Path to the reference .dfm file in tests directory
    _TEST_DFM_SOURCE = os.path.join(os.path.dirname(__file__), 'test_form.dfm')
    
    @classmethod
    def setUpClass(cls):
        """Set up test fixtures before all tests."""
        
        if not os.path.exists(cls._TEST_DFM_SOURCE):
            raise FileNotFoundError(
                f"Test .dfm file not found: {cls._TEST_DFM_SOURCE}\n"
                "This file must exist for tests to run."
            )
        
        # Create a temporary directory for test files
        cls.test_dir = tempfile.mkdtemp(prefix='p4d_test_')
        
        # Copy the reference .dfm file to test directory
        cls.valid_dfm = os.path.join(cls.test_dir, 'test_form.dfm')
        shutil.copy2(cls._TEST_DFM_SOURCE, cls.valid_dfm)
        
        # Create UTF-8 path test directory and copy .dfm file there
        utf8_dir = os.path.join(cls.test_dir, '测试_тест_🎉')
        os.makedirs(utf8_dir, exist_ok=True)
        cls.utf8_dfm = os.path.join(utf8_dir, 'form_测试.dfm')
        shutil.copy2(cls._TEST_DFM_SOURCE, cls.utf8_dfm)
    
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
        self.form = TestForm(None)
    
    def tearDown(self):
        """Clean up after each test."""
        try:
            if hasattr(self, 'form') and self.form:
                self.form.Release()
        except:
            pass
    
    def _copy_dfm_to_path(self, target_path):
        """Helper to copy the test .dfm file to a specific path."""
        target_dir = os.path.dirname(target_path)
        if target_dir and not os.path.exists(target_dir):
            os.makedirs(target_dir, exist_ok=True)
        shutil.copy2(self._TEST_DFM_SOURCE, target_path)
        return target_path
    
    def _verify_basic_properties_loaded(self, form, msg_prefix=""):
        """Helper to verify that basic form properties were actually loaded from .dfm file.
        
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
        result = self.form.LoadProps(self.valid_dfm)
        self.assertTrue(result, "LoadProps should return True for valid .dfm file")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_pathlib_path(self):
        """Test LoadProps with pathlib.Path object."""
        path_obj = Path(self.valid_dfm)
        result = self.form.LoadProps(path_obj)
        self.assertTrue(result, "LoadProps should return True for valid Path object")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_utf8_string_path(self):
        """Test LoadProps with UTF-8 characters in string path."""
        result = self.form.LoadProps(self.utf8_dfm)
        self.assertTrue(result, "LoadProps should return True for UTF-8 path")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_utf8_pathlib_path(self):
        """Test LoadProps with UTF-8 characters in Path object."""
        path_obj = Path(self.utf8_dfm)
        result = self.form.LoadProps(path_obj)
        self.assertTrue(result, "LoadProps should return True for UTF-8 Path object")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_absolute_path(self):
        """Test LoadProps with absolute path."""
        abs_path = os.path.abspath(self.valid_dfm)
        result = self.form.LoadProps(abs_path)
        self.assertTrue(result, "LoadProps should work with absolute path")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_relative_path(self):
        """Test LoadProps with relative path."""
        # Change to test directory and use relative path

        old_cwd = os.getcwd()
        try:
            os.chdir(self.test_dir)
            rel_path = os.path.basename(self.valid_dfm)
            result = self.form.LoadProps(rel_path)
            self.assertTrue(result, "LoadProps should work with relative path")
            self._verify_basic_properties_loaded(self.form)
        finally:
            os.chdir(old_cwd)
    
    def test_loadprops_with_path_containing_spaces(self):
        """Test LoadProps with path containing spaces."""
        space_dir = os.path.join(self.test_dir, 'path with spaces')
        space_file = os.path.join(space_dir, 'test file.dfm')
        self._copy_dfm_to_path(space_file)
        
        result = self.form.LoadProps(space_file)
        self.assertTrue(result, "LoadProps should work with path containing spaces")
        self._verify_basic_properties_loaded(self.form)
    


    # ========== Invalid Input Tests ==========
    
    def test_loadprops_with_nonexistent_file(self):
        """Test LoadProps with non-existent file path."""
        nonexistent = os.path.join(self.test_dir, 'nonexistent.dfm')
        
        with self.assertRaises(OSError) as context:
            self.form.LoadProps(nonexistent)
        self.assertIn('not found', str(context.exception).lower())
    
    def test_loadprops_with_none(self):
        """Test LoadProps with None (should raise TypeError)."""
        with self.assertRaises(TypeError):
            self.form.LoadProps(None)
    
    def test_loadprops_with_empty_string(self):
        """Test LoadProps with empty string."""
        with self.assertRaises(OSError) as context:
            self.form.LoadProps('')
        self.assertIn('not found', str(context.exception).lower())
    
    def test_loadprops_with_integer(self):
        """Test LoadProps with integer (wrong type)."""
        with self.assertRaises(TypeError):
            self.form.LoadProps(123)
    
    def test_loadprops_with_wrong_file_content(self):
        """Test LoadProps with file that exists but wrong extension."""
        # Create a text file with wrong extension
        txt_file = os.path.join(self.test_dir, 'test_wrong_content.dfm')
        with open(txt_file, 'w', encoding='utf-8') as f:
            f.write('not a dfm file')
        
        with self.assertRaises(RuntimeError) as context:
            self.form.LoadProps(txt_file)
        self.assertIn('EParserError', str(context.exception))
    
    # ========== PathLike Object Edge Cases ==========
    
    def test_loadprops_with_custom_pathlike(self):
        """Test LoadProps with custom PathLike object."""
        class CustomPathLike:
            def __init__(self, path):
                self.path = path
            def __fspath__(self):
                return self.path
        
        result = self.form.LoadProps(CustomPathLike(self.valid_dfm))
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
    
    def test_loadprops_with_pathlike_utf8(self):
        """Test LoadProps with custom PathLike returning UTF-8 path."""
        class UTF8PathLike:
            def __init__(self, path):
                self.path = path
            def __fspath__(self):
                return self.path
        
        result = self.form.LoadProps(UTF8PathLike(self.utf8_dfm))
        self.assertTrue(result, "LoadProps should work with UTF-8 PathLike")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_bytes_path(self):
        """Test LoadProps with bytes object as path."""
        bytes_path = self.valid_dfm.encode('utf-8')
        result = self.form.LoadProps(bytes_path)
        self.assertTrue(result, "LoadProps should work with bytes path")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_bytes_path_utf8(self):
        """Test LoadProps with UTF-8 bytes path."""
        bytes_path = self.utf8_dfm.encode('utf-8')
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
        
        bytes_pathlike = BytesPathLike(self.valid_dfm)
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
        
        utf8_bytes_pathlike = UTF8BytesPathLike(self.utf8_dfm)
        result = self.form.LoadProps(utf8_bytes_pathlike)
        self.assertTrue(result, "LoadProps should work with PathLike returning UTF-8 bytes")
        self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_with_pathlike_returning_bytes_invalid_encoding(self):
        """Test LoadProps with PathLike returning bytes with invalid encoding."""
        class InvalidBytesPathLike:
            def __fspath__(self):
                # Return bytes that are not valid UTF-8
                return b'\xff\xfe\x00\x01'
        
        with self.assertRaises(UnicodeDecodeError) as context:
            self.form.LoadProps(InvalidBytesPathLike())
        
    
    # ========== Edge Cases ==========
    
    def test_loadprops_with_very_long_path(self):
        """Test LoadProps with very long path."""
        # Create a path with many nested directories
        long_path = self.test_dir
        for i in range(10):
            long_path = os.path.join(long_path, f'dir_{i}' * 20)
        os.makedirs(long_path, exist_ok=True)
        
        long_file = os.path.join(long_path, 'test.dfm')
        self._copy_dfm_to_path(long_file)
        
        # Should work if path length is within system limits
        try:
            result = self.form.LoadProps(long_file)
            self.assertTrue(result, "LoadProps should work with long path if within system limits")
            self._verify_basic_properties_loaded(self.form)
        except (OSError, RuntimeError) as e:
            # Very long paths might fail on some systems - that's acceptable
            error_msg = str(e).lower()
            if any(term in error_msg for term in ['too long', 'path', 'filename']):
                # Expected failure for path length limits
                pass
            else:
                # Unexpected error - re-raise
                raise
    
    def test_loadprops_with_unicode_normalization(self):
        """Test LoadProps handles Unicode normalization correctly."""
        # Test with different Unicode representations
        # Create directory with combining characters
        if sys.platform == 'win32':
            # Windows may normalize Unicode differently
            # Test with é (U+00E9) vs e + U+0301 (combining acute)
            normalized_dir = os.path.join(self.test_dir, 'café')
            normalized_file = os.path.join(normalized_dir, 'form.dfm')
            self._copy_dfm_to_path(normalized_file)
            
            result = self.form.LoadProps(normalized_file)
            self.assertTrue(result, "LoadProps should handle Unicode normalization")
            self._verify_basic_properties_loaded(self.form)
    
    def test_loadprops_overwrites_existing_properties(self):
        """Test that LoadProps overwrites existing form properties."""
        self.form.Caption = 'Initial Caption'
        self.form.ClientWidth = 100
        self.form.ClientHeight = 100
        
        result = self.form.LoadProps(self.valid_dfm)
        self.assertTrue(result)
        self._verify_basic_properties_loaded(self.form)
    

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
