using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace XUnitTestProject1
{
    public class UnitTest1
    {
        [Theory]
        [InlineData("vb", new[] { "-v" })]
        public void ShouldBeVerboseWithVFlag(string pattern, string[] args)
        {
            var result = Parse(pattern, args);
            Assert.True(result.GetBoolean('v'));
        }

        [Fact]
        public void ShouldBeNotVerboseWithoutVFlag()
        {
            var pattern = "vb";

            var result = Parse(pattern);

            Assert.False(result.GetBoolean('v'));
        }

        [Fact]
        public void ShouldGetErrorWhenForUnkownFlag()
        {
            var pattern = "vb";

            var result = Parse(pattern);

            try
            {
                result.GetBoolean('x');
                Assert.True(false);
            }
            catch (UnknownFlag exception)
            {
                Assert.Equal('x', exception.Flag);
            }

        }

        [Theory]
        [InlineData("-p")]
        [InlineData("-v -p")]
        public void ShouldGetZeroForUndefinedPort(string args)
        {
            var pattern = "pi vb";

            var result = Parse(pattern, args.Split(" "));

            Assert.Equal(0, result.GetInteger('p'));
        }

        [Theory]
        [InlineData("-p 3000")]
        [InlineData("-v -p 3000")]
        public void ShouldGetDefinedPort(string args)
        {
            var pattern = "pi vb";

            var result = Parse(pattern, args.Split(" "));

            Assert.Equal(3000, result.GetInteger('p'));
        }
        

        private Args Parse(string pattern, params string[] args)
        {
            return new Args(pattern, args);
        }
    }

    internal class UnknownFlag : Exception
    {
        public UnknownFlag(char flag)
        {
            this.Flag = flag;
        }
        public char Flag { get; }
    }

    public class Args
    {
        private string[] args;
        private string pattern;

        public Args(string pattern, string[] args)
        {
            this.args = args;
            this.pattern = pattern;
        }

        public bool GetBoolean(char flag)
        {
            if (!this.pattern.Contains(flag))
            {
                throw new UnknownFlag(flag);
            }

            return args.Any();
        }

        internal int GetInteger(char flag)
        {
            var flagIndex = args.ToList().IndexOf("-" + flag);


            return args.Length > flagIndex + 1 ? int.Parse(args[flagIndex + 1]) : 0;
        }
    }
}
