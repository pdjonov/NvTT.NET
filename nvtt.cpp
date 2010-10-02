/************************************************************************************

	Copyright (c) 2010 Phill Djonov

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.

************************************************************************************/

#pragma unmanaged

#include "nvtt/nvtt.h"

#pragma managed

#include <vcclr.h>
#include <string.h>

using namespace System;

namespace Nvidia
{
namespace TextureTools
{

public enum class Format
{
	// No compression.
	Rgb = nvtt::Format_RGB,
	Rgba = nvtt::Format_RGBA,
	
	// DX9 formats.
	Dxt1 = nvtt::Format_DXT1,
	Dxt1a = nvtt::Format_DXT1a,   // DXT1 with binary alpha.
	Dxt3 = nvtt::Format_DXT3,
	Dxt5 = nvtt::Format_DXT5,
	Dxt5n = nvtt::Format_DXT5n,   // Compressed HILO: R=1, G=y, B=0, A=x
	
	// DX10 formats.
	Bc1 = nvtt::Format_BC1,
	Bc1a = nvtt::Format_BC1a,
	Bc2 = nvtt::Format_BC2,
	Bc3 = nvtt::Format_BC3,
	Bc3n = nvtt::Format_BC3n,
	Bc4 = nvtt::Format_BC4,     // ATI1
	Bc5 = nvtt::Format_BC5,     // 3DC, ATI2
};

public enum class Quality
{
	Fastest = nvtt::Quality_Fastest,
	Normal = nvtt::Quality_Normal,
	Production = nvtt::Quality_Production,
	Highest = nvtt::Quality_Highest,
};

public value struct ColorWeights
{
	property float Red;
	property float Green;
	property float Blue;
	property float Alpha;

	ColorWeights( float red, float green, float blue, float alpha )
	{
		Red = red;
		Green = green;
		Blue = blue;
		Alpha = alpha;
	}

	ColorWeights( float red, float green, float blue )
	{
		Red = red;
		Green = green;
		Blue = blue;
		Alpha = 1.0F;
	}

	static property ColorWeights One { ColorWeights get() { return ColorWeights( 1, 1, 1 ); } }

internal:
	void ToNvOptions( nvtt::CompressionOptions &opts )
	{
		opts.setColorWeights( Red, Green, Blue, Alpha );
	}
};

public value struct PixelFormat
{
	property int BitCount
	{
		int get() { return bitCount; }
		void set( int value )
		{
			switch( value )
			{
			case 8:
			case 16:
			case 24:
			case 32:
				break;

			default:
				throw gcnew ArgumentException( "BitCount must be 8, 16, 24, or 32." );
			}

			bitCount = value; 
		}
	}

	property unsigned int RedMask;
	property unsigned int GreenMask;
	property unsigned int BlueMask;
	property unsigned int AlphaMask;

	PixelFormat( int bitCount, unsigned int redMask, unsigned int greenMask, unsigned int blueMask, unsigned int alphaMask )
	{
		BitCount = bitCount;
		RedMask = redMask;
		GreenMask = greenMask;
		BlueMask = blueMask;
		AlphaMask = alphaMask;
	}

	bool CheckIsValid()
	{
		switch( bitCount )
		{
		case 8:
		case 16:
		case 24:
		case 32:
			break;

		default:
			return false;
		}

		if( (RedMask & GreenMask) || (RedMask & BlueMask) || (RedMask & AlphaMask) ||
			(GreenMask & BlueMask) || (GreenMask & AlphaMask) ||
			(BlueMask & AlphaMask) )
			return false;

		if( bitCount != 32 )
		{
			unsigned int maxMask = 1 << bitCount;

			if( RedMask > maxMask || GreenMask > maxMask ||
				BlueMask > maxMask || AlphaMask > maxMask )
				return false;
		}

		return true;
	}

	void Validate()
	{
		switch( bitCount )
		{
		case 8:
		case 16:
		case 24:
		case 32:
			break;

		default:
			throw gcnew ArgumentException( "BitCount must be 8, 16, 24, or 32." );
		}

		if( (RedMask & GreenMask) || (RedMask & BlueMask) || (RedMask & AlphaMask) ||
			(GreenMask & BlueMask) || (GreenMask & AlphaMask) ||
			(BlueMask & AlphaMask) )
			throw gcnew ArgumentException( "The channel masks must not have any bits in common." );

		if( bitCount != 32 )
		{
			unsigned int maxMask = 1 << bitCount;

			if( RedMask > maxMask || GreenMask > maxMask ||
				BlueMask > maxMask || AlphaMask > maxMask )
				throw gcnew ArgumentException( "The channel masks must not include bits beyond BitCount." );
		}
	}

	static property PixelFormat Argb8 { PixelFormat get() { return PixelFormat( 32, 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000 ); } }

private:
	int bitCount;

internal:
	void ToNvOptions( nvtt::CompressionOptions &opts )
	{
		Validate();

		opts.setPixelFormat( bitCount, RedMask, GreenMask, BlueMask, AlphaMask );
	}
};

public value struct Quantization
{
	property bool ColorDithering;
	property bool AlphaDithering;
	property Nullable< int > BinaryAlphaThreshold
	{
		Nullable< int > get() { return binaryAlphaThresh; }
		void set( Nullable< int > value )
		{
			if( value.HasValue && (value.Value < 0 || value.Value > 255) )
				throw gcnew ArgumentOutOfRangeException( "The alpha threshold must be null or between 0 and 255, inclusive." );
		}
	}

	Quantization( bool colorDithering, bool alphaDithering, Nullable< int > binaryAlphaThreshold )
	{
		ColorDithering = colorDithering;
		AlphaDithering = alphaDithering;
		BinaryAlphaThreshold = binaryAlphaThreshold;
	}

	Quantization( Nullable< int > binaryAlphaThreshold )
	{
		ColorDithering = false;
		AlphaDithering = false;
		BinaryAlphaThreshold = binaryAlphaThreshold;
	}

	static property Quantization None { Quantization get() { return Quantization(); } }
	static property Quantization BinaryAlpha { Quantization get() { return Quantization( 127 ); } }

private:
	Nullable< int > binaryAlphaThresh;

internal:
	void ToNvOptions( nvtt::CompressionOptions &opts )
	{
		if( binaryAlphaThresh.HasValue )
			opts.setQuantization( ColorDithering, AlphaDithering, true, binaryAlphaThresh.Value );
		else
			opts.setQuantization( ColorDithering, AlphaDithering, false );
	}
};

public ref class CompressionOptions sealed
{
public:
	CompressionOptions()
	{
		Reset();
	}

	void Reset()
	{
		Format = TextureTools::Format::Dxt1;
		Quality = TextureTools::Quality::Normal;
		ColorWeights = TextureTools::ColorWeights::One;
		PixelFormat = TextureTools::PixelFormat::Argb8;
		Quantization = TextureTools::Quantization::None;
	}

	property Format Format;
	property Quality Quality;
	property ColorWeights ColorWeights;
	property String^ ExternalCompressor;
	property PixelFormat PixelFormat;
	property Quantization Quantization;

internal:	
	void ToNvOptions( nvtt::CompressionOptions &opts )
	{
		opts.setFormat( (nvtt::Format)Format );
		opts.setQuality( (nvtt::Quality)Quality );

		if( ExternalCompressor )
		{
			int len = Text::Encoding::ASCII->GetByteCount( ExternalCompressor );
			array< unsigned char > ^str = gcnew array< unsigned char >( len + 1 );
			Text::Encoding::ASCII->GetBytes( ExternalCompressor, 0, ExternalCompressor->Length, str, 0 );
			str[str->Length - 1] = 0;
			
			pin_ptr< unsigned char > pStr = &str[0];

			opts.setExternalCompressor( (char*)pStr );
		}
		else
		{
			opts.setExternalCompressor( "" );
		}

		ColorWeights.ToNvOptions( opts );
		PixelFormat.ToNvOptions( opts );
		Quantization.ToNvOptions( opts );
	}
};

public enum class WrapMode
{
	Clamp = nvtt::WrapMode_Clamp,
	Repeat = nvtt::WrapMode_Repeat,
	Mirror = nvtt::WrapMode_Mirror,
};

public enum class TextureType
{
	Texture2D = nvtt::TextureType_2D,
	TextureCube = nvtt::TextureType_Cube,
};

public enum class InputFormat
{
	Bgra_8ub = nvtt::InputFormat_BGRA_8UB,
};

public enum class MipmapFilter
{
	Box = nvtt::MipmapFilter_Box,       ///< Box filter is quite good and very fast.
	Triangle = nvtt::MipmapFilter_Triangle,  ///< Triangle filter blurs the results too much, but that might be what you want.
	Kaiser = nvtt::MipmapFilter_Kaiser,    ///< Kaiser-windowed Sinc filter is the best downsampling filter.
};

public enum class ColorTransform
{
	None = nvtt::ColorTransform_None,
	Linear = nvtt::ColorTransform_Linear,
};

public enum class RoundMode
{
	None = nvtt::RoundMode_None,
	ToNextPowerOfTwo = nvtt::RoundMode_ToNextPowerOfTwo,
	ToNearestPowerOfTwo = nvtt::RoundMode_ToNearestPowerOfTwo,
	ToPreviousPowerOfTwo = nvtt::RoundMode_ToPreviousPowerOfTwo,
};

public enum class AlphaMode
{
	None = nvtt::AlphaMode_None,
	Transparency = nvtt::AlphaMode_Transparency,
	Premultiplied = nvtt::AlphaMode_Premultiplied,
};

public value struct InputDescription
{
	property InputFormat Format;
	property TextureType TextureType;

	property int Width
	{
		int get() { return width; }
		void set( int value )
		{
			if( value < 0 )
				throw gcnew ArgumentOutOfRangeException();

			width = value;
		}
	}

	property int Height
	{
		int get() { return height; }
		void set( int value )
		{
			if( value < 0 )
				throw gcnew ArgumentOutOfRangeException();

			height = value;
		}
	}

	property int Depth
	{
		int get() { return depth; }
		void set( int value )
		{
			if( value < 0 )
				throw gcnew ArgumentOutOfRangeException();

			depth = value;
		}
	}

	InputDescription( TextureTools::TextureType textureType, int width, int height, int depth )
	{
		TextureType = textureType;
		Width = width;
		Height = height;
		Depth = depth;
	}

	InputDescription( TextureTools::TextureType textureType, int width, int height )
	{
		TextureType = textureType;
		Width = width;
		Height = height;
		Depth = 1;
	}

private:
	int width, height, depth;
};

public value struct KaiserParameters
{
	property float Width;
	property float Alpha;
	property float Stretch;

	KaiserParameters( float width, float alpha, float stretch )
	{
		Width = width;
		Alpha = alpha;
		Stretch = stretch;
	}
};

public value struct MipmapGenerator
{
	property MipmapFilter Filter;
	property Nullable< int > MaxLevel
	{
		Nullable< int > get() { return maxLevel; }
		void set( Nullable< int > value )
		{
			if( value.HasValue && value.Value < 0 )
				throw gcnew ArgumentOutOfRangeException();

			maxLevel = value;
		}
	}

	property KaiserParameters KaiserParameters;

	MipmapGenerator( MipmapFilter filter )
	{
		Filter = filter;
	}

	MipmapGenerator( MipmapFilter filter, int maxLevel )
	{
		if( maxLevel < 0 )
			throw gcnew ArgumentOutOfRangeException();

		Filter = filter;
		MipmapGenerator::maxLevel = maxLevel;
	}

	MipmapGenerator( TextureTools::KaiserParameters kaiserParameters, int maxLevel )
	{
		if( maxLevel < 0 )
			throw gcnew ArgumentOutOfRangeException();

		Filter = MipmapFilter::Kaiser;
		KaiserParameters = kaiserParameters;
		MipmapGenerator::maxLevel = maxLevel;
	}

	MipmapGenerator( TextureTools::KaiserParameters kaiserParameters )
	{
		Filter = MipmapFilter::Kaiser;
		KaiserParameters = kaiserParameters;
	}

private:
	Nullable< int > maxLevel;
};

public value struct NormalFilter
{
	property float Small;
	property float Medium;
	property float Big;
	property float Large;

	NormalFilter( float small, float medium, float big, float large )
	{
		Small = small;
		Medium = medium;
		Big = big;
		Large = large;
	}
};

public value struct NormalMapConverter
{
	property ColorWeights HeightEvaluation;
	property NormalFilter NormalFilter;
	property bool NormalizeMipmaps;

	NormalMapConverter( ColorWeights heightEvaluation, TextureTools::NormalFilter normalFilter, bool normalizeMipmaps )
	{
		HeightEvaluation = heightEvaluation;
		NormalFilter = normalFilter;
		NormalizeMipmaps = normalizeMipmaps;
	}
};

public value struct ResizeMode
{
	property RoundMode Rounding;
	property Nullable< int > MaxExtents
	{
		Nullable< int > get() { return maxExtents ? Nullable< int >( maxExtents ) : Nullable< int >(); }
		void set( Nullable< int > value )
		{
			if( value.HasValue && value.Value <= 0 )
				throw gcnew ArgumentOutOfRangeException();

			maxExtents = value.GetValueOrDefault( 0 );
		}
	}

	ResizeMode( RoundMode rounding, int maxExtents )
	{
		Rounding = rounding;
		MaxExtents = maxExtents;
	}

	ResizeMode( RoundMode rounding )
	{
		Rounding = rounding;
		MaxExtents = 0;
	}

	ResizeMode( int maxExtents )
	{
		Rounding = RoundMode::None;
		MaxExtents = maxExtents;
	}

private:
	int maxExtents;
};

public ref class InputObject sealed
{
public:
	InputObject()
	{
		opts = new nvtt::InputOptions;
		Reset();
	}

	InputObject( InputDescription inputDescription )
	{
		opts = new nvtt::InputOptions;
		
		Reset();
		
		ChangeInputDescription( inputDescription );
	}

	~InputObject()
	{
		InputObject::!InputObject();
	}

#pragma region Image data

	property bool HasInputDescription
	{
		bool get()
		{
			CheckDisposed();
			return layout.HasValue;
		}
	}

	property TextureTools::InputDescription InputDescription
	{
		TextureTools::InputDescription get()
		{
			CheckDisposed();

			if( !layout.HasValue )
				throw gcnew InvalidOperationException();

			return layout.Value;
		}
	}

	void ChangeInputDescription( TextureTools::InputDescription newInputDescription )
	{
		CheckDisposed();

		opts->setFormat( (nvtt::InputFormat)newInputDescription.Format );
		opts->setTextureLayout( (nvtt::TextureType)newInputDescription.TextureType,
			newInputDescription.Width, newInputDescription.Height, newInputDescription.Depth );

		layout = newInputDescription;
	}

	void DiscardInputData()
	{
		CheckDisposed();

		opts->resetTextureLayout();

		layout = Nullable< TextureTools::InputDescription >();
	}

	void SetData( int face, int mipLevel, IntPtr data, int width, int height, int depth )
	{
		if( data == IntPtr::Zero )
			throw gcnew ArgumentNullException();

		if( face < 0 || mipLevel < 0 || width < 0 || height < 0 || depth != 1 )
			throw gcnew ArgumentOutOfRangeException();

		CheckDisposed();

		if( !layout.HasValue )
			throw gcnew InvalidOperationException( "You must initialize the InputDescription before setting data." );

		TextureTools::InputDescription desc = layout.Value;

		switch( desc.TextureType )
		{
		case TextureType::Texture2D:
			if( face != 0 )
				throw gcnew ArgumentOutOfRangeException( "face must be zero for 2D textures" );
			break;

		case TextureType::TextureCube:
			if( face >= 6 )
				throw gcnew ArgumentOutOfRangeException();
			break;
		}

		int w = desc.Width;
		int h = desc.Height;
		int d = desc.Depth;

		for( int i = 0; i < mipLevel; i++ )
		{
			if( w == 1 && h == 1 && d == 1 )
				//can't go down another mip level, but not done iterating
				throw gcnew ArgumentOutOfRangeException( "mipLevel" );

			w /= 2; if( !w ) w = 1;
			h /= 2; if( !h ) w = 1;
			d /= 2; if( !d ) w = 1;
		}

		if( width != w || height != h || depth != d )
			throw gcnew ArgumentOutOfRangeException( "Mismatched mip level data size." );

		if( !opts->setMipmapData( data.ToPointer(), width, height, depth, face, mipLevel ) )
			throw gcnew ArgumentException();
	}

	generic< typename T >
	where T : ValueType
	void SetData( int face, int mipLevel, array< T > ^data, int index, int count, int width, int height, int depth )
	{
		if( !data )
			throw gcnew ArgumentNullException();

		if( index < 0 || count < 0 || count > data->Length - index )
			throw gcnew ArgumentOutOfRangeException();

		if( face < 0 || mipLevel < 0 || width < 0 || height < 0 || depth != 1 )
			throw gcnew ArgumentOutOfRangeException();

		CheckDisposed();

		if( !layout.HasValue )
			throw gcnew InvalidOperationException( "You must initialize the InputDescription before setting data." );

		TextureTools::InputDescription desc = layout.Value;

		int dt;

		switch( desc.Format )
		{
		case InputFormat::Bgra_8ub:
		default:
			dt = 4;
			break;
		}

		if( width * height * depth * dt > count * (int)sizeof( T ) )
			throw gcnew ArgumentException( "Array range is smaller than the required data." );

		pin_ptr< T > pinData = &data[index];
		SetData( face, mipLevel, (IntPtr)pinData, width, height, depth );
	}

	generic< typename T >
	where T : ValueType
	void SetData( int face, int mipLevel, array< T > ^data, int width, int height, int depth )
	{
		if( !data )
			throw gcnew ArgumentNullException();

		SetData< T >( face, mipLevel, data, 0, data->Length, width, height, depth );
	}

#pragma endregion

	property Nullable< MipmapGenerator > MipmapGenerator
	{
		Nullable< TextureTools::MipmapGenerator > get()
		{
			CheckDisposed();

			return mipGen;
		}

		void set( Nullable< TextureTools::MipmapGenerator > value )
		{
			CheckDisposed();

			if( value.HasValue )
			{
				TextureTools::MipmapGenerator gen = value.Value;
				opts->setMipmapGeneration( true, gen.MaxLevel.GetValueOrDefault( -1 ) );
				opts->setMipmapFilter( (nvtt::MipmapFilter)gen.Filter );
				opts->setKaiserParameters( gen.KaiserParameters.Width, gen.KaiserParameters.Alpha, gen.KaiserParameters.Stretch );
			}
			else
			{
				opts->setMipmapGeneration( false );
			}

			mipGen = value;
		}
	}

	property bool IsNormalMap
	{
		bool get()
		{
			CheckDisposed();
			return isNormalMap;
		}

		void set( bool value )
		{
			CheckDisposed();

			opts->setNormalMap( value );

			isNormalMap = value;
		}
	}
	
	property Nullable< NormalMapConverter > NormalMapConverter
	{
		Nullable< TextureTools::NormalMapConverter > get()
		{
			CheckDisposed();

			return normConv;
		}

		void set( Nullable< TextureTools::NormalMapConverter > value )
		{
			CheckDisposed();

			if( value.HasValue )
			{
				TextureTools::NormalMapConverter conv = value.Value;

				opts->setConvertToNormalMap( true );
				opts->setHeightEvaluation( conv.HeightEvaluation.Red, conv.HeightEvaluation.Green,
					conv.HeightEvaluation.Blue, conv.HeightEvaluation.Alpha );
				opts->setNormalFilter( conv.NormalFilter.Small, conv.NormalFilter.Medium,
					conv.NormalFilter.Big, conv.NormalFilter.Large );
				opts->setNormalizeMipmaps( conv.NormalizeMipmaps );
			}
			else
			{
				opts->setConvertToNormalMap( false );
			}

			normConv = value;
		}
	}

	
	property WrapMode WrapMode
	{
		TextureTools::WrapMode get()
		{
			CheckDisposed();
			return wrap; 
		}

		void set( TextureTools::WrapMode value )
		{
			CheckDisposed();

			opts->setWrapMode( (nvtt::WrapMode)value );

			wrap = value;
		}
	}

	property AlphaMode AlphaMode
	{
		TextureTools::AlphaMode get()
		{
			CheckDisposed();
			return amode; 
		}

		void set( TextureTools::AlphaMode value )
		{
			CheckDisposed();

			opts->setAlphaMode( (nvtt::AlphaMode)value );

			amode = value;
		}
	}

	//color transform is apparently unimplemented

	property float InputGamma
	{
		float get()
		{
			CheckDisposed();
			return inGamma;
		}

		void set( float value )
		{
			CheckDisposed();

			opts->setGamma( value, outGamma );

			inGamma = value;
		}
	}

	property float OutputGamma
	{
		float get()
		{
			CheckDisposed();
			return outGamma;
		}

		void set( float value )
		{
			CheckDisposed();

			opts->setGamma( inGamma, value );

			outGamma = value;
		}
	}

	property Nullable< ResizeMode > ResizeMode
	{
		Nullable< TextureTools::ResizeMode > get()
		{
			CheckDisposed();
			return resize;
		}

		void set( Nullable< TextureTools::ResizeMode > value )
		{
			CheckDisposed();

			if( value.HasValue )
			{
				TextureTools::ResizeMode rm = value.Value;

				opts->setRoundMode( (nvtt::RoundMode)rm.Rounding );
				opts->setMaxExtents( rm.MaxExtents.GetValueOrDefault( 0 ) );
			}
			else
			{
				opts->setRoundMode( nvtt::RoundMode_None );
				opts->setMaxExtents( 0 );
			}

			resize = value;
		}
	}

	void Reset()
	{
		CheckDisposed();

		WrapMode = TextureTools::WrapMode::Mirror;
		AlphaMode = TextureTools::AlphaMode::None;

		opts->setGamma( 2.2F, 2.2F );

		inGamma = 2.2F;
		outGamma = 2.2F;

		TextureTools::MipmapGenerator mipGen( MipmapFilter::Box );
		mipGen.KaiserParameters = KaiserParameters( 3, 4, 1 );
		MipmapGenerator = mipGen;

		IsNormalMap = false;
		NormalMapConverter = Nullable< TextureTools::NormalMapConverter >();

		ResizeMode = Nullable< TextureTools::ResizeMode >();
	}

protected:
	!InputObject()
	{
		delete opts;
		opts = 0;
	}

	void CheckDisposed()
	{
		if( !opts )
			throw gcnew ObjectDisposedException( "InputOptions" );
	}

internal:
	nvtt::InputOptions* GetUnderlyingObject()
	{
		CheckDisposed();
		return opts;
	}

private:
	nvtt::InputOptions *opts;
	Nullable< TextureTools::InputDescription > layout;
	Nullable< TextureTools::MipmapGenerator > mipGen;
	Nullable< TextureTools::NormalMapConverter > normConv;
	Nullable< TextureTools::ResizeMode > resize;
	TextureTools::WrapMode wrap;
	TextureTools::AlphaMode amode;
	float inGamma, outGamma;
	bool isNormalMap;
};

public interface class IOutputHandler
{
public:
	void BeginImage( int face, int mipLevel, int byteSize, int width, int height, int depth );
	void WriteData( array< unsigned char > ^data );
};

public interface class IUnsafeOutputHandler
{
public:
	void BeginImage( int face, int mipLevel, int byteSize, int width, int height, int depth );
	void WriteData( IntPtr pData, int size );
};

public enum class CompressionError
{
	Unknown = nvtt::Error_Unknown,
	InvalidInput = nvtt::Error_InvalidInput,
	UnsupportedFeature = nvtt::Error_UnsupportedFeature,
	CudaError = nvtt::Error_CudaError,
	FileOpen = nvtt::Error_FileOpen,
	FileWrite = nvtt::Error_FileWrite,
};

public ref class CompressionErrorEventArgs : EventArgs
{
public:
	property CompressionError Error { CompressionError get() { return error; } }

	CompressionErrorEventArgs( CompressionError error )
	{
		CompressionErrorEventArgs::error = error;
	}

private:
	CompressionError error;
};

public delegate void CompressionErrorEventHandler( Object ^sender, CompressionErrorEventArgs ^e );

ref class Compressor;

namespace impl
{

struct ErrorHandler : public nvtt::ErrorHandler
{
	ErrorHandler( Compressor ^owner )
	{
		ErrorHandler::owner = owner;
	}

	/* override */ virtual void error( nvtt::Error e );
	
private:
	gcroot< Compressor^ > owner;
};

struct OutputHandler : public nvtt::OutputHandler
{
	OutputHandler( IOutputHandler ^target )
	{
		OutputHandler::target = target;
	}

	/* override */ virtual void beginImage( int size, int width, int height, int depth, int face, int miplevel )
	{
		try
		{
			target->BeginImage( face, miplevel, size, width, height, depth );
		}
		catch( ... )
		{
			//don't pass these up to the CPP layer or bad things happen
		}
	}

	/* override */ virtual bool writeData( const void *data, int size )
	{
		try
		{
			array< unsigned char > ^buf = gcnew array< unsigned char >( size );

			{
				pin_ptr< unsigned char > pBuf = &buf[0];
				memcpy( pBuf, data, size );
			}

			target->WriteData( buf );
		}
		catch( ... )
		{
			return false;
		}

		return true;
	}

private:
	gcroot< IOutputHandler^ > target;
};

struct UnsafeOutputHandler : public nvtt::OutputHandler
{
	UnsafeOutputHandler( IUnsafeOutputHandler ^target )
	{
		UnsafeOutputHandler::target = target;
	}

	/* override */ virtual void beginImage( int size, int width, int height, int depth, int face, int miplevel )
	{
		try
		{
			target->BeginImage( face, miplevel, size, width, height, depth );
		}
		catch( ... )
		{
			//don't pass these up to the CPP layer or bad things happen
		}
	}

	/* override */ virtual bool writeData( const void *data, int size )
	{
		try
		{
			target->WriteData( (IntPtr)const_cast< void* >( data ), size );
		}
		catch( ... )
		{
			return false;
		}

		return true;
	}

private:
	gcroot< IUnsafeOutputHandler^ > target;
};

struct StreamOutputHandler : public nvtt::OutputHandler
{
	StreamOutputHandler( IO::Stream ^target )
	{
		StreamOutputHandler::target = target;
	}

	/* override */ virtual void beginImage( int size, int width, int height, int depth, int face, int miplevel )
	{
	}

	/* override */ virtual bool writeData( const void *data, int size )
	{
		try
		{
			array< unsigned char > ^buf = buffer;
			
			if( !buf || buf->Length < size )
			{
				buf = gcnew array< unsigned char >( Math::Max( size, 1024 ) );
				buffer = buf;
			}

			{
				pin_ptr< unsigned char > pBuf = &buf[0];
				memcpy( pBuf, data, size );
			}

			target->Write( buf, 0, size );
		}
		catch( ... )
		{
			return false;
		}

		return true;
	}

private:
	gcroot< array< unsigned char >^ > buffer;
	gcroot< IO::Stream^ > target;
};

}

public ref class Compressor sealed
{
public:
	Compressor()
	{
		cmpr = new nvtt::Compressor;

		EnableCudaAcceleeration = false;
	}

	~Compressor()
	{
		Compressor::!Compressor();
	}

	property bool EnableCudaAcceleeration
	{
		bool get()
		{
			CheckDisposed();

			return cmpr->isCudaAccelerationEnabled();
		}
		
		void set( bool value )
		{
			CheckDisposed();
			
			cmpr->enableCudaAcceleration( value );
		}
	}

	int EstimateSize( InputObject ^input, CompressionOptions ^options )
	{
		if( !input || !options )
			throw gcnew ArgumentNullException();

		CheckDisposed();

		nvtt::InputOptions *in = input->GetUnderlyingObject();

		nvtt::CompressionOptions opts;
		options->ToNvOptions( opts );

		return cmpr->estimateSize( *in, opts );
	}

	void Process( InputObject ^input, CompressionOptions ^options, IOutputHandler ^output )
	{
		if( !input || !options || !output )
			throw gcnew ArgumentNullException();

		CheckDisposed();

		nvtt::InputOptions *in = input->GetUnderlyingObject();

		nvtt::CompressionOptions opts;
		options->ToNvOptions( opts );

		nvtt::OutputOptions outOpts;

		impl::OutputHandler out( output );
		outOpts.setOutputHandler( &out );

		impl::ErrorHandler err( this );
		outOpts.setErrorHandler( &err );

		if( !cmpr->process( *in, opts, outOpts ) )
			throw gcnew Exception();
	}

	void Process( InputObject ^input, CompressionOptions ^options, IUnsafeOutputHandler ^output )
	{
		if( !input || !options || !output )
			throw gcnew ArgumentNullException();

		CheckDisposed();

		nvtt::InputOptions *in = input->GetUnderlyingObject();

		nvtt::CompressionOptions opts;
		options->ToNvOptions( opts );

		nvtt::OutputOptions outOpts;

		impl::UnsafeOutputHandler out( output );
		outOpts.setOutputHandler( &out );

		impl::ErrorHandler err( this );
		outOpts.setErrorHandler( &err );

		if( !cmpr->process( *in, opts, outOpts ) )
			throw gcnew Exception();
	}

	void Process( InputObject ^input, CompressionOptions ^options, String ^outputFile )
	{
		if( !input || !options || !outputFile )
			throw gcnew ArgumentNullException();

		CheckDisposed();

		nvtt::InputOptions *in = input->GetUnderlyingObject();

		nvtt::CompressionOptions opts;
		options->ToNvOptions( opts );

		nvtt::OutputOptions outOpts;

		{
			int len = Text::Encoding::ASCII->GetByteCount( outputFile );
			array< unsigned char > ^str = gcnew array< unsigned char >( len + 1 );
			Text::Encoding::ASCII->GetBytes( outputFile, 0, outputFile->Length, str, 0 );
			str[str->Length - 1] = 0;
			
			pin_ptr< unsigned char > pStr = &str[0];
			outOpts.setFileName( (const char*)pStr );
		}

		impl::ErrorHandler err( this );
		outOpts.setErrorHandler( &err );

		if( !cmpr->process( *in, opts, outOpts ) )
			throw gcnew Exception();
	}

	void Process( InputObject ^input, CompressionOptions ^options, IO::Stream ^output )
	{
		if( !input || !options || !output )
			throw gcnew ArgumentNullException();

		if( !output->CanWrite )
			throw gcnew ArgumentException( "output must be writeable" );

		CheckDisposed();

		nvtt::InputOptions *in = input->GetUnderlyingObject();

		nvtt::CompressionOptions opts;
		options->ToNvOptions( opts );

		nvtt::OutputOptions outOpts;

		impl::StreamOutputHandler out( output );
		outOpts.setOutputHandler( &out );

		impl::ErrorHandler err( this );
		outOpts.setErrorHandler( &err );

		if( !cmpr->process( *in, opts, outOpts ) )
			throw gcnew Exception();
	}

	event CompressionErrorEventHandler ^CompressionError;

protected:
	!Compressor()
	{
		delete cmpr;
		cmpr = 0;
	}

	void CheckDisposed()
	{
		if( !cmpr )
			throw gcnew ObjectDisposedException( "Compressor" );
	}

public protected:

	void OnCompressionError( CompressionErrorEventArgs ^e )
	{
		CompressionError( this, e );
	}

private:
	nvtt::Compressor *cmpr;
};


namespace impl
{

/* override */ void ErrorHandler::error( nvtt::Error e )
{
	try
	{
		owner->OnCompressionError( gcnew CompressionErrorEventArgs( (CompressionError)e ) );
	}
	catch( ... )
	{
		//can't send these up to the CPP layer
	}
}

}

}
}